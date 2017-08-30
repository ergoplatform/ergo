package org.ergoplatform.mining


import org.bouncycastle.crypto.digests.{Blake2bDigest, SHA256Digest}
import org.ergoplatform.crypto.Equihash
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.ModifierId
import org.ergoplatform.utils.LittleEndianBytes.leIntToByteArray
import scorex.core.block.Block._
import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.control.NonFatal
import scala.util.{Random, Try}

object Miner extends ScorexLogging {

  type Solution = Seq[Int]

  // add chain byte?
  private def ergoPerson(n: Int, k: Int): Array[Byte] = "ERGOPoWT".getBytes ++ leIntToByteArray(n) ++ leIntToByteArray(k)

  def genBlock(nBits: Long,
               parent: Header,
               stateRoot: Array[Byte],
               adProofs: ADProof,
               transactions: Seq[AnyoneCanSpendTransaction],
               timestamp: Timestamp,
               votes: Array[Byte],
               n: Int, k: Int): ErgoFullBlock = {
    val h = genHeader(nBits,
      parent, stateRoot, adProofs.proofBytes, BlockTransactions.rootHash(transactions.map(_.id)), votes, timestamp, n, k)
    new ErgoFullBlock(h, BlockTransactions(h.id, transactions), None)
  }

  def isBlockValid(block: ErgoFullBlock, n: Int, k: Int): Boolean = Try {
    val result = Equihash.validateSolution(n, k, ergoPerson(n, k), HeaderSerializer.bytesWithoutNonceAndSolutions(block.header) ++ Equihash.nonceToLeBytes(block.header.nonce), EquihashSolutionsSerializer.parseBytes(block.header.equihashSolutions).get)
    result
  }.recover {
    case NonFatal(e) =>
      log.debug(s"Block ${block.id} is invalid due pow", e)
      false
  }.getOrElse(false)

  def genHeader(nBits: Long,
                parent: Header,
                stateRoot: Array[Byte],
                adProofsRoot: Array[Byte],
                transactionsRoot: Array[Byte],
                votes: Array[Byte],
                timestamp: Timestamp,
                n: Int, k: Int): Header = {
    val interlinks: Seq[Array[Byte]] = if (parent.isGenesis) constructInterlinkVector(parent) else Seq(parent.id)
    val difficulty = RequiredDifficulty.decodeCompactBits(nBits)
    val height = parent.height + 1

    val bytesPerWord = n / 8
    val wordsPerHash = 512 / n

    val digest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, ergoPerson(n, k))
    val h = Header(0.toByte, parent.id, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits, height, votes, 0l, null)
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

    val interinks = generateInterlinks(Constants.InitialDifficulty * 2, Seq[Array[Byte]]())
    assert(interinks.length >= parentInterlinks.length - 1)

    genesisId +: interinks

  }

  private def constructInterlinkVector(parent: Header): Seq[Array[Byte]] = if (parent.isGenesis) {
    Seq(parent.id)
  } else {
    constructInterlinks(parent.interlinks,
      parent.realDifficulty,
      parent.id)
  }

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = {
    realDifficulty >= difficulty
  }

}
