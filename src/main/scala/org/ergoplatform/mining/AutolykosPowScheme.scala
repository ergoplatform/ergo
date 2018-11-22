package org.ergoplatform.mining

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Algos
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.Try

/**
  * Autolykos PoW puzzle.
  *
  * @see papers/yellow/pow/ErgoPow.tex for full description
  * @param k - number of elements in one solution
  * @param N - list size
  */
class AutolykosPowScheme(k: Int, N: Int) extends ScorexLogging {

  // Constant data added to hash function to increase it's calculation time
  val M: Array[Byte] = Array.fill(2 * 1024)(0: Byte)

  private var list: IndexedSeq[BigInt] = IndexedSeq()
  private var x: BigInt = randomSecret()
  private var lastInitMsg: Array[Byte] = Array()

  /**
    * Checks that `header` contains correct solution of the Autolykos PoW puzzle.
    */
  def validate(header: Header): Try[Unit] = Try {
    val b = getB(header.nBits)
    val msg = msgByHeader(header)
    val s = header.powSolution

    require(s.d < b || s.d > (q - b), s"Incorrect d=${s.d} for b=$b")
    require(s.pk.getCurve == group.curve && !s.pk.isInfinity, "pk is incorrect")
    require(s.w.getCurve == group.curve && !s.w.isInfinity, "w is incorrect")

    val p1 = pkToBytes(s.pk)
    val p2 = pkToBytes(s.w)
    val f = genIndexes(msg, s.n).map(ib => genElement(msg, p1, p2, Ints.toByteArray(ib))).sum.mod(q)
    val left = s.w.multiply(f.bigInteger).add(s.pk.negate())
    val right = group.exponentiate(group.generator, s.d.bigInteger)

    require(left == right, "Incorrect points")
  }

  /**
    * Real difficulty of `header`.
    * May occasionally exceeds required difficulty due to random nature of PoW puzzle.
    */
  def realDifficulty(header: Header): BigInt = {
    q / header.powSolution.d
  }

  /**
    * Find a nonce from `minNonce` to `maxNonce`, such that header with the specified fields will contain
    * correct solution of the Autolykos PoW puzzle.
    */
  def prove(parentOpt: Option[Header],
            nBits: Long,
            stateRoot: ADDigest,
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
    val msg = msgByHeader(h)
    val b = getB(nBits)
    initializeIfNeeded(msg, sk, b)
    checkNonces(msg, sk, b, minNonce, maxNonce).map(s => h.copy(powSolution = s))
  }

  /**
    * Get message we should proof for header `h`
    */
  def msgByHeader(h: Header): Array[Byte] = Algos.hash(HeaderSerializer.bytesWithoutPow(h))

  /**
    * Find a nonce from `minNonce` to `maxNonce`, such that full block with the specified fields will contain
    * correct solution of the Autolykos PoW puzzle.
    */
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

  /**
    * Find a nonce from `minNonce` to `maxNonce`, such that full block created from block candidate `candidateBlock`
    * will contain correct solution of the Autolykos PoW puzzle.
    */
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

  /**
    * Initialize the PoW task if it was not initialized yet.
    * Generate a new random secret `x` and fill `list` with numbers if difficulty is big enough
    *
    * @param m  - header bytes without pow
    * @param sk - secret key
    * @param b  - difficulty
    */
  private[mining] def initializeIfNeeded(m: Array[Byte],
                                         sk: BigInt,
                                         b: BigInt): Unit = if (!java.util.Arrays.equals(m, lastInitMsg)) {
    x = randomSecret()
    lastInitMsg = m
    if (!onFlyCalculation(b)) {
      log.debug(s"Initialize PoW task by generating list of $N elements")
      val p1 = pkToBytes(genPk(sk))
      val p2 = pkToBytes(genPk(x))
      list = (0 until N).map { i =>
        if (i % 1000000 == 0 && i > 0) log.debug(s"$i generated")
        genElement(m, p1, p2, Ints.toByteArray(i))
      }
    }
  }

  /**
    * Check, that on-fly calculation is more profitable
    */
  private def onFlyCalculation(b: BigInt): Boolean = N > (q / b)

  private[mining] def checkNonces(m: Array[Byte], sk: BigInt, b: BigInt, startNonce: Long, endNonce: Long): Option[AutolykosSolution] = {
    log.debug(s"Going to check nonces from $startNonce to $endNonce")

    @tailrec
    def loop(i: Long, getFullElement: Int => BigInt): Option[AutolykosSolution] = if (i == endNonce) {
      None
    } else {
      if (i % 1000000 == 0 && i > 0) log.debug(s"$i nonce tested")
      val nonce = Longs.toByteArray(i)
      val d = (x * genIndexes(m, nonce).map(i => getFullElement(i)).sum - sk).mod(q)
      if (d <= b) {
        log.debug(s"Solution found at $i")
        Some(AutolykosSolution(genPk(sk), genPk(x), nonce, d))
      } else {
        loop(i + 1, getFullElement)
      }
    }

    if (onFlyCalculation(b)) {
      val p1 = pkToBytes(genPk(sk))
      val p2 = pkToBytes(genPk(x))
      loop(startNonce, i => genElement(m, p1, p2, Ints.toByteArray(i)))
    } else {
      loop(startNonce, list)
    }
  }

  private[mining] def getB(nBits: Long): BigInt = {
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

  /**
    * Hash function that takes `m` and `nonceBytes` and returns a list of size `k` with numbers in
    * [0,`N`)
    */
  private def genIndexes(m: Array[Byte], nonceBytes: Array[Byte]): Seq[Int] = {
    val seed = Bytes.concat(m, nonceBytes)
    val hashesRequired = (k.toDouble / 8).ceil.toInt
    val indexes = (0 until hashesRequired) flatMap { i =>
      val hash = Blake2b256(Bytes.concat(seed, Ints.toByteArray(i)))
      hash.grouped(4).map(b => Math.abs(Ints.fromByteArray(b) % N))
    }
    indexes.take(k)
  }.ensuring(_.length == k)

  /**
    * Generate element of Autolykos equation.
    */
  private def genElement(m: Array[Byte],
                         pk: Array[Byte],
                         w: Array[Byte],
                         indexBytes: Array[Byte]): BigInt = {
    hash(Bytes.concat(indexBytes, M, pk, m, w))
  }

}
