package org.ergoplatform.mining

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.block.Block
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{ModifierId, ScorexLogging}
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.Try

/**
  * Autolykos PoW puzzle scheme reference implementation.
  * Mining process is implemented in inefficient way and should not be used in real environment.
  *
  * @see papers/yellow/pow/ErgoPow.tex for full description
  * @param k - number of elements in one solution
  * @param n - power of number of elements in a list
  */
class AutolykosPowScheme(val k: Int, val n: Int) extends ScorexLogging {

  assert(k <= 32, "k > 32 is not allowed due to genIndexes function")
  assert(n < 31, "n >= 31 is not allowed")

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

    require(s.d < b, s"Incorrect d = ${s.d} for b = $b")
    require(s.pk.getCurve == group.curve && !s.pk.isInfinity, "pk is incorrect")
    require(s.w.getCurve == group.curve && !s.w.isInfinity, "w is incorrect")

    val p1 = groupElemToBytes(s.pk)
    val p2 = groupElemToBytes(s.w)
    val f = genIndexes(Bytes.concat(msg, s.n)).map(ib => genElement(msg, p1, p2, Ints.toByteArray(ib))).sum.mod(q)
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
            version: Block.Version,
            nBits: Long,
            stateRoot: ADDigest,
            adProofsRoot: Digest32,
            transactionsRoot: Digest32,
            timestamp: Timestamp,
            extensionHash: Digest32,
            votes: Array[Byte],
            sk: PrivateKey,
            minNonce: Long = Long.MinValue,
            maxNonce: Long = Long.MaxValue): Option[Header] = {
    val (parentId, height) = AutolykosPowScheme.derivedHeaderFields(parentOpt)

    val h = Header(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, null, votes)
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
                 version: Block.Version,
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[ErgoTransaction],
                 timestamp: Timestamp,
                 extensionCandidate: ExtensionCandidate,
                 votes: Array[Byte],
                 sk: PrivateKey,
                 minNonce: Long = Long.MinValue,
                 maxNonce: Long = Long.MaxValue): Option[ErgoFullBlock] = {

    val transactionsRoot = BlockTransactions.transactionsRoot(transactions)
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)
    val extensionRoot: Digest32 = Extension.rootHash(extensionCandidate)

    prove(parentOpt, version, nBits, stateRoot, adProofsRoot, transactionsRoot,
      timestamp, extensionRoot, votes, sk, minNonce, maxNonce).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      val blockTransactions = BlockTransactions(h.id, transactions)
      val extension = extensionCandidate.toExtension(h.id)
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
      candidateBlock.version,
      candidateBlock.nBits,
      candidateBlock.stateRoot,
      candidateBlock.adProofBytes,
      candidateBlock.transactions,
      candidateBlock.timestamp,
      candidateBlock.extension,
      candidateBlock.votes,
      sk,
      minNonce,
      maxNonce
    )
  }

  /**
    * Assembles `ErgoFullBlock` using candidate block and external pow solution.
    */
  def completeBlock(candidate: CandidateBlock, solution: AutolykosSolution): ErgoFullBlock = {
    val header = AutolykosPowScheme.deriveUnprovedHeader(candidate).copy(powSolution = solution)
    val adProofs = ADProofs(header.id, candidate.adProofBytes)
    val blockTransactions = BlockTransactions(header.id, candidate.transactions)
    val extension = Extension(header.id, candidate.extension.fields)
    new ErgoFullBlock(header, blockTransactions, extension, Some(adProofs))
  }

  /**
    * Assembles candidate block derivative required for external miner.
    */
  def deriveExternalCandidate(candidate: CandidateBlock, pk: ProveDlog): ExternalCandidateBlock = {
    val h = AutolykosPowScheme.deriveUnprovedHeader(candidate)
    val msg = msgByHeader(h)
    val b = getB(candidate.nBits)

    ExternalCandidateBlock(msg, b, pk)
  }

  /**
    * Check nonces from `startNonce` to `endNonce` for message `m`, secrets `sk` and `x`, difficulty `b`.
    * Return AutolykosSolution if there is any valid nonce in this interval.
    */
  private[mining] def checkNonces(m: Array[Byte], sk: BigInt, x: BigInt, b: BigInt, startNonce: Long, endNonce: Long): Option[AutolykosSolution] = {
    log.debug(s"Going to check nonces from $startNonce to $endNonce")
    val p1 = groupElemToBytes(genPk(sk))
    val p2 = groupElemToBytes(genPk(x))

    @tailrec
    def loop(i: Long): Option[AutolykosSolution] = if (i == endNonce) {
      None
    } else {
      if (i % 1000000 == 0 && i > 0) log.debug(s"$i nonce tested")
      val nonce = Longs.toByteArray(i)
      val d = (x * genIndexes(Bytes.concat(m, nonce)).map(i => genElement(m, p1, p2, Ints.toByteArray(i))).sum - sk).mod(q)
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
  private[mining] def getB(nBits: Long): BigInt = q / RequiredDifficulty.decodeCompactBits(nBits)

  /**
    * Hash function that takes `m` and `nonceBytes` and returns a list of size `k` with numbers in
    * [0,`N`)
    */
  private def genIndexes(seed: Array[Byte]): Seq[Int] = {
    val hash = Blake2b256(seed)
    val extendedHash = Bytes.concat(hash, hash.take(3))
    (0 until k).map { i =>
      BigInt(1, extendedHash.slice(i, i + 4)).mod(N).toInt
    }
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

object AutolykosPowScheme {

  /**
    * Calculate header fields based on parent header
    */
  def derivedHeaderFields(parentOpt: Option[Header]): (ModifierId, Int) = {

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(ErgoHistory.GenesisHeight)

    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(Header.GenesisParentId)

    (parentId, height)
  }

  /**
    * Derives header without pow from [[CandidateBlock]].
    */
  def deriveUnprovedHeader(candidate: CandidateBlock): Header = {
    val (parentId, height) = derivedHeaderFields(candidate.parentOpt)
    val transactionsRoot = BlockTransactions.transactionsRoot(candidate.transactions)
    val adProofsRoot = ADProofs.proofDigest(candidate.adProofBytes)
    val extensionRoot: Digest32 = Extension.rootHash(candidate.extension)

    Header(
      candidate.version,
      parentId,
      adProofsRoot,
      candidate.stateRoot,
      transactionsRoot,
      candidate.timestamp,
      candidate.nBits,
      height,
      extensionRoot,
      null,
      candidate.votes
    )
  }

}
