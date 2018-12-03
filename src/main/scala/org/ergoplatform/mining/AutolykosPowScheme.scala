package org.ergoplatform.mining

import com.google.common.primitives.{Bytes, Ints, Longs}
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

/**
  * Autolykos PoW puzzle implementation.
  * Mining process is implemented in inefficient way and should not be used in real environment.
  *
  * @see papers/yellow/pow/ErgoPow.tex for full description
  * @param k - number of elements in one solution
  * @param n - power of number of elements in a list
  */
class AutolykosPowScheme(val k: Int, val n: Int) extends ScorexLogging {

  assert(n < 31, "n >= 31 is not allowed")
  assert(k > 4, "n <= 4 is not allowed")

  /**
    * Total number of elements
    */
  private val N: Int = Math.pow(2, n).toInt

  /**
    * Constant data to be added to hash function to increase it's calculation time
    */
  val M: Array[Byte] = (0 until 1024).toArray.flatMap(i => Longs.toByteArray(i))

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
    val getElement: Int => BigInt = i => genElement(msg, p1, p2, Longs.toByteArray(i))
    val f = genElements(s.n, getElement).sum.mod(q)
    val left = s.w.multiply(f.bigInteger)
    val right = group.generator.multiply(s.d.bigInteger).add(s.pk)

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
    val x = randomSecret()
    checkNonces(msg, sk, x, b, minNonce, maxNonce).map(s => h.copy(powSolution = s))
  }

  /**
    * Get message we should proof for header `h`
    */
  def msgByHeader(h: Header): Array[Byte] = Blake2b256(HeaderSerializer.bytesWithoutPow(h))

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
    * Check nonces from `startNonce` to `endNonce` for message `m`, secrets `sk` and `x`, difficulty `b`.
    * Return AutolykosSolution if there is any valid nonce in this interval.
    */
  private[mining] def checkNonces(m: Array[Byte], sk: BigInt, x: BigInt, b: BigInt, startNonce: Long, endNonce: Long): Option[AutolykosSolution] = {
    log.debug(s"Going to check nonces from $startNonce to $endNonce")
    val p1 = pkToBytes(genPk(sk))
    val p2 = pkToBytes(genPk(x))
    val getElement: Int => BigInt = i => genElement(m, p1, p2, Longs.toByteArray(i))

    @tailrec
    def loop(i: Long): Option[AutolykosSolution] = if (i == endNonce) {
      None
    } else {
      if (i % 1000000 == 0 && i > 0) log.debug(s"$i nonce tested")
      val nonce = Longs.toByteArray(i)
      val d = (x * genElements(nonce, getElement).sum - sk).mod(q)
      if (d <= b) {
        log.debug(s"Solution found at $i")
        Some(AutolykosSolution(genPk(sk), genPk(x), nonce, d))
      } else {
        loop(i + 1)
      }
    }

    loop(startNonce)
  }

  /**
    * Get target `b` from encoded difficulty `nBits`
    */
  private[mining] def getB(nBits: Long): BigInt = {
    q / RequiredDifficulty.decodeCompactBits(nBits)
  }

  /**
    * Calculate header fields based on parent header
    */
  protected def derivedHeaderFields(parentOpt: Option[Header]): (ModifierId, Byte, Seq[ModifierId], Int) = {
    val interlinks: Seq[ModifierId] =
      parentOpt.map(parent => new PoPoWProofUtils(this).constructInterlinkVector(parent)).getOrElse(Seq.empty)

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(ErgoHistory.GenesisHeight)

    val version = Header.CurrentVersion

    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(Header.GenesisParentId)

    (parentId, version, interlinks, height)
  }

  /**
    * Generate element of Autolykos equation.
    */
  private def genElement(msg: Array[Byte],
                         pk: Array[Byte],
                         w: Array[Byte],
                         indexBytes: Array[Byte]): BigInt = {
    hash(Bytes.concat(indexBytes, M, pk, msg, w))
  }

  /**
    * Generate k elements for concrete nonce.
    * First 4 elements are chosen deterministic from the nonce.
    * The rest elements are chosen using xor of last 4 bytes of these elements as a seed,
    * mixing last 4 bytes of a newly generated element at every step
    */
  private def genElements(nonce: Array[Byte],
                          getElement: Int => BigInt): Seq[BigInt] = {
    assert(nonce.length == 8)
    val doubledNonce = Bytes.concat(nonce, nonce)
    val first4: Seq[BigInt] = (0 until 4) map { i =>
      getElement(Math.abs(Ints.fromByteArray(doubledNonce.slice(i * 2, i * 2 + 4))) % N)
    }
    val seed = first4.map(_.toByteArray.takeRight(4)).reduce((a, b) => xor(a, b))

    @tailrec
    def elementsGeneration(acc: Seq[BigInt], seed: Array[Byte]): Seq[BigInt] = if (acc.length >= k) {
      acc
    } else {
      val index = Math.abs(Ints.fromByteArray(seed)) % N
      val element = getElement(index)
      val nextSeed = xor(element.toByteArray.takeRight(4), seed)
      elementsGeneration(element +: acc, nextSeed)
    }

    elementsGeneration(first4, seed)
  }.ensuring(_.length == k)


  private def xor(ha: Array[Byte], hb: Array[Byte]): Array[Byte] = {
    ha.zip(hb).map(z => (z._1 ^ z._2).toByte)
  }
}
