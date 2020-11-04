package org.ergoplatform.mining

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
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
  *
  * See https://docs.ergoplatform.com/ErgoPow.pdf for details
  *
  * Mining process is implemented in inefficient way and should not be used in real environment.
  *
  * @see papers/yellow/pow/ErgoPow.tex for full description
  * @param k - number of elements in one solution
  * @param n - power of number of elements in a list
  */
class AutolykosPowScheme(val k: Int, val n: Int) extends ScorexLogging {

  assert(k <= 32, "k > 32 is not allowed due to genIndexes function")
  assert(n < 31, "n >= 31 is not allowed")

  //Consensus-critical code below

  /**
    * Total number of elements
    */
  private val N: Int = Math.pow(2, n).toInt

  /**
    * Constant data to be added to hash function to increase its calculation time
    */
  val M: Array[Byte] = (0 until 1024).toArray.flatMap(i => Longs.toByteArray(i))

  /**
    * Checks that `header` contains correct solution of the Autolykos PoW puzzle.
    */
  def validate(header: Header): Try[Unit] = Try {
    val version = header.version

    val b = getB(header.nBits)
    val msg = msgByHeader(header)
    val s = header.powSolution

    val pkBytes = if (version == 1) {
      require(s.d < b, s"Incorrect d = ${s.d} for b = $b")
      require(s.pk.getCurve == group.curve && !s.pk.isInfinity, "pk is incorrect")
      groupElemToBytes(s.pk)
    } else {
      //todo: fix realDifficulty (needed for nipopows) for Header
      Array.emptyByteArray
    }
    val wBytes = if (version == 1) {
      require(s.w.getCurve == group.curve && !s.w.isInfinity, "w is incorrect")
      groupElemToBytes(s.w)
    } else {
      Array.emptyByteArray
    }
    val nonce = s.n

    val seed = Bytes.concat(msg, nonce) // Autolykos v1/2, Alg. 2, line4: m || nonce
    val indexes = genIndexes(seed)

    val f = if (version == 1) {
      indexes.map(idx => genElement(version, msg, pkBytes, wBytes, Ints.toByteArray(idx))).sum.mod(q)
    } else {
      //pk and w not used in v2
      indexes.map(idx => genElement(version, msg, pkBytes, wBytes, Ints.toByteArray(idx))).sum
    }

    if (version == 1) {
      val left = s.w.multiply(f.bigInteger)
      val right = group.generator.multiply(s.d.bigInteger).add(s.pk)
      require(left == right, "Incorrect points")
    } else {
      // sum as byte array is always about 32 bytes
      val array: Array[Byte] = BigIntegers.asUnsignedByteArray(32,  f.underlying())
      require(toBigInt(hash(array)) < b, "h(f) >= b")
    }
  }

  /**
    * Real difficulty of `header`.
    * May occasionally exceeds required difficulty due to random nature of PoW puzzle.
    * Used in NiPoPoW.
    */
  def realDifficulty(header: Header): BigInt = {
    q / header.powSolution.d
  }

  /**
    * Header digest ("message" for default GPU miners) a miner is working on
    */
  def msgByHeader(h: HeaderWithoutPow): Array[Byte] = Blake2b256(HeaderSerializer.bytesWithoutPow(h))

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
  private def genElement(version: Block.Version,
                         m: Array[Byte],
                         pk: Array[Byte],  // not used in v2
                         w: Array[Byte],   // not used in v2
                         indexBytes: Array[Byte]): BigInt = {
    if (version == 1) {
      // Autolykos v. 1: H(j|M|pk|m|w) (line 5 from the Algo 2 of the spec)
      hashModQ(Bytes.concat(indexBytes, M, pk, m, w))
    } else {
      // Autolykos v. 2: H(j|M|m) (line 5 from the Algo 2 of the spec)
      toBigInt(hash(Bytes.concat(indexBytes, M, m)).drop(1))
    }
  }

  //Proving-related code which is not critical for consensus below

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

    val h = HeaderWithoutPow(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, votes)
    val msg = msgByHeader(h)
    val b = getB(nBits)
    val x = randomSecret()
    checkNonces(version, msg, sk, x, b, minNonce, maxNonce).map(solution => h.toHeader(solution))
  }

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

    prove(parentOpt, version, nBits, stateRoot, adProofsRoot, transactionsRoot,
      timestamp, extensionCandidate.digest, votes, sk, minNonce, maxNonce).map { h =>
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
    * Check nonces from `startNonce` to `endNonce` for message `m`, secrets `sk` and `x`, difficulty `b`.
    * Return AutolykosSolution if there is any valid nonce in this interval.
    */
  private[mining] def checkNonces(version: Block.Version,
                                  m: Array[Byte],
                                  sk: BigInt,
                                  x: BigInt,
                                  b: BigInt,
                                  startNonce: Long,
                                  endNonce: Long): Option[AutolykosSolution] = {
    log.debug(s"Going to check nonces from $startNonce to $endNonce")
    val p1 = groupElemToBytes(genPk(sk))
    val p2 = groupElemToBytes(genPk(x))

    @tailrec
    def loop(i: Long): Option[AutolykosSolution] = if (i == endNonce) {
      None
    } else {
      if (i % 1000000 == 0 && i > 0) log.debug(s"$i nonce tested")
      val nonce = Longs.toByteArray(i)
      val seed = Bytes.concat(m, nonce)
      val d = if(version == 1) {
        (x * genIndexes(seed).map(i => genElement(version, m, p1, p2, Ints.toByteArray(i))).sum - sk).mod(q)
      } else {
        val indexes = genIndexes(seed)
        toBigInt(hash(indexes.map(i => genElement(version, m, p1, p2, Ints.toByteArray(i))).sum.toByteArray))
      }
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
    * Assembles candidate block for external miners.
    */
  def deriveExternalCandidate(candidate: CandidateBlock, pk: ProveDlog): WorkMessage =
    deriveExternalCandidate(candidate, pk, Seq.empty)

  /**
    * Assembles candidate block for external miner with certain transactions included into the block
    *
    * @param candidate      - block candidate (contained the transactions)
    * @param pk             - miner pubkey
    * @param mandatoryTxIds - ids of the transactions to include
    * @return - block candidate for external miner
    */
  def deriveExternalCandidate(candidate: CandidateBlock,
                              pk: ProveDlog,
                              mandatoryTxIds: Seq[ModifierId]): WorkMessage = {
    val h = ErgoMiner.deriveUnprovenHeader(candidate)
    val msg = msgByHeader(h)
    val b = getB(candidate.nBits)
    val v = candidate.version

    val proofs = if (mandatoryTxIds.nonEmpty) {
      // constructs fake block transactions section (BlockTransactions instance) to get proofs from it
      val fakeHeaderId = scorex.util.bytesToId(Array.fill(org.ergoplatform.wallet.Constants.ModifierIdLength)(0: Byte))
      val bt = BlockTransactions(fakeHeaderId, candidate.transactions)
      val ps = mandatoryTxIds.flatMap { txId => bt.proofFor(txId).map(mp => TransactionMembershipProof(txId, mp)) }
      Some(ProofOfUpcomingTransactions(h, ps))
    } else {
      None
    }
    WorkMessage(msg, b, pk, v, proofs)
  }

}

object AutolykosPowScheme {

  /**
    * Calculate header fields based on its parent, namely, header's parent id and height
    */
  def derivedHeaderFields(parentOpt: Option[Header]): (ModifierId, Int) = {

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(ErgoHistory.GenesisHeight)

    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(Header.GenesisParentId)

    (parentId, height)
  }

}
