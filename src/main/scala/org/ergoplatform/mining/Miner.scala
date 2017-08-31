package org.ergoplatform.mining

import com.google.common.primitives.Chars
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.ergoplatform.crypto.Equihash
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.block.Block._
import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.control.NonFatal
import scala.util.{Random, Try}


object Miner extends ScorexLogging {

  type Solution = Seq[Int]

  //todo: add chain byte?
  private def ergoPerson(n: Char, k: Char): Array[Byte] =
    "ERGOPoWT".getBytes ++ Chars.toByteArray(n) ++ Chars.toByteArray(k)

  def genBlock(nBits: Long,
               parent: Option[Header],
               stateRoot: Array[Byte],
               adProofs: ADProof,
               transactions: Seq[AnyoneCanSpendTransaction],
               timestamp: Timestamp,
               votes: Array[Byte],
               n: Char, k: Char): ErgoFullBlock = {
    val h = genHeader(nBits, parent, stateRoot, adProofs.proofBytes,
      BlockTransactions.rootHash(transactions.map(_.id)), votes, timestamp, n, k)
    new ErgoFullBlock(h, BlockTransactions(h.id, transactions), None)
  }

  def isValidBlock(block: ErgoFullBlock, n: Char, k: Char): Boolean = Try {
    Equihash.validateSolution(n, k, ergoPerson(n, k),
      HeaderSerializer.bytesWithoutNonceAndSolutions(block.header) ++ Equihash.nonceToLeBytes(block.header.nonce),
      EquihashSolutionsSerializer.parseBytes(block.header.equihashSolutions).get)
  }.recover {
    case NonFatal(e) =>
      log.debug(s"Block ${block.id} is invalid due pow", e)
      false
  }.getOrElse(false)

  //todo: description
  def genHeader(nBits: Long,
                parentOpt: Option[Header],
                stateRoot: Array[Byte],
                adProofsRoot: Array[Byte],
                transactionsRoot: Array[Byte],
                votes: Array[Byte],
                timestamp: Timestamp,
                n: Char,
                k: Char): Header = {
    val interlinks: Seq[Array[Byte]] = if (parentOpt.isDefined) constructInterlinkVector(parentOpt.get) else Seq()
    val difficulty = RequiredDifficulty.decodeCompactBits(nBits)
    val height = parentOpt.map(parent => parent.height + 1).getOrElse(0)

    val bytesPerWord = n / 8
    val wordsPerHash = 512 / n

    val digest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, ergoPerson(n, k))
    val h = Header(0.toByte, parentOpt.map(_.id).getOrElse(Header.GenesisParentId), interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits, height, votes, 0l, null)
    val I = HeaderSerializer.bytesWithoutNonceAndSolutions(h)
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
        correctWorkDone(newHeader.realDifficulty, difficulty)
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

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = {
    realDifficulty >= difficulty
  }

  //todo: there's constructInterlinkVector() method already in PoPoWPoof class
  private def constructInterlinkVector(parent: Header): Seq[Array[Byte]] = if (parent.isGenesis) {
    Seq(parent.id)
  } else {
    constructInterlinks(parent.interlinks,
      parent.realDifficulty,
      parent.id)
  }

  private[mining] def constructInterlinks(parentInterlinks: Seq[Array[Byte]],
                                          parentRealDifficulty: BigInt,
                                          parentId: ModifierId): Seq[Array[Byte]] = {

    val genesisId = parentInterlinks.head

    def generateInterlinks(curDifficulty: BigInt, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = {
      if (parentRealDifficulty >= curDifficulty) {
        generateInterlinks(curDifficulty * 2, acc :+ parentId)
      } else {
        parentInterlinks.tail.find(pId => Algos.blockIdDifficulty(pId) >= curDifficulty) match {
          case Some(id) => generateInterlinks(curDifficulty * 2, acc :+ id)
          case _ => acc
        }
      }
    }

    val interlinks = generateInterlinks(Constants.InitialDifficulty * 2, Seq[Array[Byte]]())
    assert(interlinks.length >= parentInterlinks.length - 1)

    genesisId +: interlinks
  }
}