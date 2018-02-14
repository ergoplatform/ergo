package org.ergoplatform.mining

import com.google.common.primitives.Chars
import com.typesafe.config.ConfigException
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.ergoplatform.crypto.Equihash
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.Constants
import scorex.core.ModifierId
import scorex.core.block.Block.Timestamp
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.control.NonFatal

trait PoWScheme {

  def prove(parentOpt: Option[Header],
            nBits: Long,
            stateRoot: ADDigest,
            adProofsRoot: Digest32,
            transactionsRoot: Digest32,
            timestamp: Timestamp,
            votes: Array[Byte],
            startingNonce: Long,
            finishingNonce: Long
           ): Option[Header]

  def proveBlock(parentOpt: Option[Header],
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[AnyoneCanSpendTransaction],
                 timestamp: Timestamp,
                 votes: Array[Byte],
                 startingNonce: Long,
                 finishingNonce: Long): Option[ErgoFullBlock] = {

    val transactionsRoot = BlockTransactions.rootHash(transactions.map(_.id))
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot,
      timestamp, votes, startingNonce, finishingNonce).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      new ErgoFullBlock(h, BlockTransactions(h.id, transactions), Some(adProofs))
    }
  }

  def proveBlock(candidateBlock: CandidateBlock,
                 nonce: Long): Option[ErgoFullBlock] = {

    val parentOpt: Option[Header] = candidateBlock.parentOpt
    val nBits: Long = candidateBlock.nBits
    val stateRoot: ADDigest = candidateBlock.stateRoot
    val adProofBytes: SerializedAdProof = candidateBlock.adProofBytes
    val transactions: Seq[AnyoneCanSpendTransaction] = candidateBlock.transactions
    val timestamp: Timestamp = candidateBlock.timestamp
    val votes: Array[Byte] = candidateBlock.votes

    val transactionsRoot = BlockTransactions.rootHash(transactions.map(_.id))
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, votes, nonce, nonce).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      new ErgoFullBlock(h, BlockTransactions(h.id, transactions), Some(adProofs))
    }
  }

  def verify(header: Header): Boolean

  def realDifficulty(header: Header): BigInt

  protected def derivedHeaderFields(parentOpt: Option[Header]): (ModifierId, Byte, Seq[ModifierId], Int) = {
    val interlinks: Seq[ModifierId] =
      parentOpt.map(parent => new PoPoWProofUtils(this).constructInterlinkVector(parent)).getOrElse(Seq.empty)

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(0)

    val version = 0: Byte

    val parentId: ModifierId = ModifierId @@ parentOpt.map(_.id).getOrElse(Header.GenesisParentId)

    (parentId, version, interlinks, height)
  }

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = {
    realDifficulty >= difficulty
  }
}

object PoWScheme {
  type Solution = Seq[Int]

  def apply(powType: String, nParam: Option[Int], kParam: Option[Int]): PoWScheme = (powType, nParam, kParam) match {
    case (DefaultFakePowScheme.schemeName, _, _) => DefaultFakePowScheme
    case (EquihashPowScheme.schemeName, Some(n), Some(k)) => new EquihashPowScheme(n.toChar, k.toChar)
    case (EquihashPowScheme.schemeName,  None , _) => throw new ConfigException.Missing("ergo.chain.poWScheme.n",
                                                      new NoSuchElementException("Missing 'n' parameter for Equihash"))
    case (EquihashPowScheme.schemeName,  _ , None) => throw new ConfigException.Missing("ergo.chain.poWScheme.k",
                                                      new NoSuchElementException("Missing 'k' parameter for Equihash"))
    case _ => throw new ConfigException.BadValue("ergo.chain.poWScheme.powType", powType)
  }
}

class EquihashPowScheme(n: Char, k: Char) extends PoWScheme with ScorexLogging {
  lazy val ergoPerson: Array[Byte] = "ERGOPoWT1234".getBytes("UTF-8") ++
    Chars.toByteArray(n) ++
    Chars.toByteArray(k)

  @SuppressWarnings(Array("NullParameter"))
  override def prove(parentOpt: Option[Header],
                     nBits: Long,
                     stateRoot: ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     votes: Array[Byte],
                     startingNonce: Long,
                     finishingNonce: Long
                    ): Option[Header] = {
    require(finishingNonce >= startingNonce)

    val difficulty = RequiredDifficulty.decodeCompactBits(nBits)

    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)

    val bytesPerWord = n / 8
    val wordsPerHash = 512 / n

    val digest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, ergoPerson) // scalastyle:ignore
    val h = Header(version, parentId, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, votes, nonce = 0L, null) // scalastyle:ignore

    val I = HeaderSerializer.bytesWithoutPow(h)
    digest.update(I, 0, I.length)

    @tailrec
    def generateHeader(nonce: Long): Option[Header] = {
      log.debug("Trying nonce: " + nonce)
      val currentDigest = new Blake2bDigest(digest)
      Equihash.hashNonce(currentDigest, nonce)
      val solutions = Equihash.gbpBasic(currentDigest, n, k)
      val headerWithSuitableSolution = solutions.map(solution => {
        h.copy(nonce = nonce, equihashSolutions = EquihashSolutionsSerializer.toBytes(solution))
      }).find(newHeader => {
        correctWorkDone(realDifficulty(newHeader), difficulty)
      })
      headerWithSuitableSolution match {
        case headerWithFoundSolution: Some[Header] =>
          headerWithFoundSolution
        case None =>
          if (nonce + 1 == finishingNonce) None else generateHeader(nonce + 1)
      }
    }

    generateHeader(startingNonce)
  }

  override def verify(header: Header): Boolean =
    EquihashSolutionsSerializer.parseBytes(header.equihashSolutions).map { solutionIndices =>
      Equihash.validateSolution(
        n,
        k,
        ergoPerson,
        HeaderSerializer.bytesWithoutPow(header) ++ Equihash.nonceToLeBytes(header.nonce),
        solutionIndices.toIndexedSeq
      )
    }.recover {
      case NonFatal(e) =>
        log.debug(s"Block ${header.id} is invalid due pow", e)
        false
    }.getOrElse(false)

  override def realDifficulty(header: Header): Difficulty = Constants.MaxTarget / BigInt(1, header.powHash)

  override def toString: String = s"EquihashPowScheme(n = ${n.toInt}, k = ${k.toInt})"
}

object EquihashPowScheme {
  val schemeName = "equihash"
}