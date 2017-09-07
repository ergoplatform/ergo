package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Chars, Ints}
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.ergoplatform.crypto.Equihash
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.Constants
import scorex.core.ModifierId
import scorex.core.block.Block.Timestamp
import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.{Random, Try}
import scala.util.control.NonFatal
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32

trait PoWScheme {
  def prove(parentOpt: Option[Header],
            nBits: Long,
            stateRoot: ADDigest,
            adProofsRoot: Digest32,
            transactionsRoot: Digest32,
            timestamp: Timestamp,
            votes: Array[Byte]): Header

  def proveBlock(parentOpt: Option[Header],
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[AnyoneCanSpendTransaction],
                 timestamp: Timestamp,
                 votes: Array[Byte]): ErgoFullBlock = {

    val transactionsRoot = BlockTransactions.rootHash(transactions.map(_.id))
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    val h = prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, votes)
    val adProofs = ADProofs(h.id, adProofBytes)

    new ErgoFullBlock(h, BlockTransactions(h.id, transactions), Some(adProofs))
  }

  def verify(header: Header): Boolean

  def realDifficulty(header: Header): BigInt

  protected def derivedHeaderFields(parentOpt: Option[Header]) = {
    val interlinks: Seq[ModifierId] =
      parentOpt.map(parent => new PoPoWProofUtils(this).constructInterlinkVector(parent)).getOrElse(Seq())

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
}


class EquihashPowScheme(n: Char, k: Char) extends PoWScheme with ScorexLogging {
  lazy val ergoPerson: Array[Byte] = "ERGOPoWT1234".getBytes ++ Chars.toByteArray(n) ++ Chars.toByteArray(k)

  override def prove(parentOpt: Option[Header],
                     nBits: Long,
                     stateRoot: ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     votes: Array[Byte]): Header = {


    val difficulty = RequiredDifficulty.decodeCompactBits(nBits)

    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)

    val bytesPerWord = n / 8
    val wordsPerHash = 512 / n

    val digest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, ergoPerson)
    val h = Header(version, parentId, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, votes, nonce = 0L, null)

    val I = HeaderSerializer.bytesWithoutPow(h)
    digest.update(I, 0, I.length)

    @tailrec
    def generateHeader(): Header = {
      val nonce = Random.nextLong()
      val currentDigest = new Blake2bDigest(digest)
      Equihash.hashNonce(currentDigest, nonce)
      val solutions = Equihash.gbpBasic(currentDigest, n, k)
      val headerWithSuitableSolution = solutions.map(solution => {
        h.copy(nonce = nonce, equihashSolutions = EquihashSolutionsSerializer.toBytes(solution))
      }).find(newHeader => {
        correctWorkDone(realDifficulty(newHeader), difficulty)
      })
      headerWithSuitableSolution match {
        case Some(headerWithFoundSolution) =>
          headerWithFoundSolution
        case None =>
          generateHeader()
      }
    }

    generateHeader()
  }

  override def verify(header: Header): Boolean =
    Try {
      Equihash.validateSolution(n, k, ergoPerson,
        HeaderSerializer.bytesWithoutPow(header) ++ Equihash.nonceToLeBytes(header.nonce),
        EquihashSolutionsSerializer.parseBytes(header.equihashSolutions).get)
    }.recover {
      case NonFatal(e) =>
        log.debug(s"Block ${header.id} is invalid due pow", e)
        false
    }.getOrElse(false)

  override def realDifficulty(header: Header): Difficulty = Constants.MaxTarget / BigInt(1, header.powHash)
}


class FakePowScheme(levelOpt:Option[Int]) extends PoWScheme {

  override def prove(parentOpt: Option[Header], nBits: Long, stateRoot: ADDigest, adProofsRoot: Digest32,
                     transactionsRoot: Digest32, timestamp: Timestamp, votes: Array[Byte]): Header = {

    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)

    val level: Int = levelOpt.map(lvl => BigInt(2).pow(lvl).toInt).getOrElse(Random.nextInt(1000) + 1)

    new Header(version, parentId, interlinks,
      adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits, height, votes,
      nonce = 0L, Ints.toByteArray(level))
  }

  override def verify(header: Header): Boolean = true

  override def realDifficulty(header: Header): Difficulty =
    Ints.fromByteArray(header.equihashSolutions) * header.requiredDifficulty
}

object DefaultFakePowScheme extends FakePowScheme(None)