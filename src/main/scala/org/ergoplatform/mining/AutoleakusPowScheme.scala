package org.ergoplatform.mining

import java.math.BigInteger

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.Try

class AutoleakusPowScheme(k: Int, N: Int) extends ScorexLogging {

  private var list: IndexedSeq[BigInt] = IndexedSeq()
  private var x: BigInt = randomSecret()

  private val NBigInteger: BigInteger = BigInt(N).bigInteger

  def verify(header: Header): Boolean = Try {
    val b = getB(header.nBits)
    val msg = HeaderSerializer.bytesWithoutPow(header)
    val s = header.powSolution

    require(s.d < b || s.d > (q - b), s"Incorrect d=${s.d} for b=$b")
    require(s.pk.getCurve == group.curve && !s.pk.isInfinity, "pk is incorrect")
    require(s.w.getCurve == group.curve && !s.w.isInfinity, "w is incorrect")
    val gExp = group.exponentiate(group.generator, (f1(msg, s.pk, s.w, s.n) - s.d).mod(q).bigInteger)
    val pkExp = s.w.multiply(f2(msg, s.pk, s.w, s.n).bigInteger)

    require(gExp.add(pkExp) == s.pk, "Incorrect points")
  }.isSuccess

  def realDifficulty(header: Header): BigInt = {
    q / header.powSolution.d
  }

  def prove(parentOpt: Option[Header],
            nBits: Long,
            stateRoot:
            ADDigest,
            adProofsRoot: Digest32,
            transactionsRoot: Digest32,
            timestamp: Timestamp,
            extensionHash: Digest32,
            sk: PrivateKey,
            minNonce: Long = Long.MinValue,
            maxNonce: Long = Long.MaxValue): Option[Header] = {
    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)

    val h = Header(version, parentId, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, null)
    val msg = HeaderSerializer.bytesWithoutPow(h)
    val b = getB(nBits)
    initialize(msg, sk)
    checkNonces(msg, sk, b, minNonce, maxNonce).map(s => h.copy(powSolution = s))
  }

  def proveBlock(parentOpt: Option[Header],
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[ErgoTransaction],
                 timestamp: Timestamp,
                 extensionCandidate: ExtensionCandidate,
                 sk: PrivateKey,
                 minNonce: Long = Long.MinValue,
                 maxNonce: Long = Long.MaxValue): Option[ErgoFullBlock] = {

    val transactionsRoot = BlockTransactions.transactionsRoot(transactions)
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)
    val extensionRoot: Digest32 = Extension.rootHash(extensionCandidate)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot,
      timestamp, extensionRoot, sk, minNonce, maxNonce).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      val blockTransactions = BlockTransactions(h.id, transactions)
      val extension = Extension(h.id, extensionCandidate.mandatoryFields, extensionCandidate.optionalFields)
      new ErgoFullBlock(h, blockTransactions, extension, Some(adProofs))
    }
  }

  def proveCandidate(candidateBlock: CandidateBlock,
                     sk: PrivateKey,
                     minNonce: Long = Long.MinValue,
                     maxNonce: Long = Long.MaxValue): Option[ErgoFullBlock] = {
    proveBlock(candidateBlock.parentOpt,
      candidateBlock.nBits,
      candidateBlock.stateRoot,
      candidateBlock.adProofBytes,
      candidateBlock.transactions,
      candidateBlock.timestamp,
      candidateBlock.extension,
      sk,
      minNonce,
      maxNonce
    )
  }

  def initialize(m: Array[Byte], sk: BigInt): Unit = {
    x = randomSecret()
    val pk = genPk(sk)
    val w = genPk(x)
    val p1 = pkToBytes(pk)
    val p2 = pkToBytes(w)
    log.debug(s"Generate list of $N elements")
    list = (0 until N).map { i =>
      if (i % 1000000 == 0 && i > 0) log.debug(s"$i generated")
      val indexBytes = Ints.toByteArray(i)
      (genElement(m, p1, p2, indexBytes, 0: Byte) + x * genElement(m, p1, p2, indexBytes, 1: Byte)).mod(q)
    }
  }

  def checkNonces(m: Array[Byte], sk: BigInt, b: BigInt, startNonce: Long, endNonce: Long): Option[AutoleakusSolution] = {
    log.debug(s"Going to check nonces from $startNonce to $endNonce")

    @tailrec
    def loop(i: Long): Option[AutoleakusSolution] = if (i == endNonce) {
      None
    } else {
      if (i % 1000000 == 0 && i > 0) log.debug(s"$i nonce tested")
      val nonce = Longs.toByteArray(i)
      val d = (genIndexes(m, nonce).map(i => list(i)).sum - sk).mod(q)
      if (d <= b) {
        log.debug(s"Solution found at $i")
        Some(AutoleakusSolution(genPk(sk), genPk(x),nonce, d))
      } else {
        loop(i + 1)
      }
    }

    log.debug(s"Start search of solution with $k elements from list with $N elements")
    loop(startNonce)
  }

  protected def getB(nBits: Long): BigInt = {
    q / RequiredDifficulty.decodeCompactBits(nBits)
  }

  protected def derivedHeaderFields(parentOpt: Option[Header]): (ModifierId, Byte, Seq[ModifierId], Int) = {
    val interlinks: Seq[ModifierId] =
      parentOpt.map(parent => new PoPoWProofUtils(this).constructInterlinkVector(parent)).getOrElse(Seq.empty)

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(ErgoHistory.GenesisHeight)

    val version = Header.CurrentVersion

    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(Header.GenesisParentId)

    (parentId, version, interlinks, height)
  }


  private def f1(m: Array[Byte], pk: ECPoint, w: ECPoint, nonce: Array[Byte]): BigInt = {
    val p1 = pkToBytes(pk)
    val p2 = pkToBytes(w)
    genIndexes(m, nonce).map(ib => genElement(m, p1, p2, Ints.toByteArray(ib), 0: Byte)).sum.mod(q)
  }

  private def f2(m: Array[Byte], pk: ECPoint, w: ECPoint, nonce: Array[Byte]): BigInt = {
    val p1 = pkToBytes(pk)
    val p2 = pkToBytes(w)
    genIndexes(m, nonce).map(ib => genElement(m, p1, p2, Ints.toByteArray(ib), 1: Byte)).sum.mod(q)
  }


  private def genIndexes(m: Array[Byte], nonceBytes: Array[Byte]): Seq[Int] = {
    val seed = Bytes.concat(m, nonceBytes)
    val hashesRequired = (k.toDouble / 8).ceil.toInt
    val indexes = (0 until hashesRequired) flatMap { i =>
      val hash = Blake2b256(Bytes.concat(seed, Ints.toByteArray(i)))
      hash.grouped(4).map(b => BigIntegers.fromUnsignedByteArray(b).mod(NBigInteger).intValue())
    }
    indexes.take(k)
  }.ensuring(_.length == k)



  /**
    * Generate element for left (orderByte = 0) or for right (orderByte = 1) part
    * of Autolykus equation.
    */
  protected def genElement(m: Array[Byte],
                           p1: Array[Byte],
                           p2: Array[Byte],
                           indexBytes: Array[Byte],
                           orderByte: Byte): BigInt = {
    hash(Bytes.concat(m, p1, p2, indexBytes, Array(orderByte)))
  }

}
