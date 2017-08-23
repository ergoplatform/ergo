package org.ergoplatform.mining


import org.bouncycastle.crypto.digests.{Blake2bDigest, SHA256Digest}
import org.ergoplatform.crypto.Equihash
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils.LittleEndianBytes.leIntToByteArray
import scorex.core.block.Block._
import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.Random

object Miner extends ScorexLogging {

  def blockHash(prevHash: Array[Byte], nonce: BigInt, soln: Seq[Int]): Array[Byte] = {
    // H(I||V||x_1||x_2||...|x_2^k)
    val digest = new SHA256Digest()
    digest.update(prevHash, 0, prevHash.length)
    Equihash.hashNonce(digest, nonce)
    soln.foreach(s => Equihash.hashXi(digest, s))
    val h = new Array[Byte](32)
    digest.doFinal(h, 0)
    val secondDigest = new SHA256Digest()
    secondDigest.update(h, 0, h.length)
    val result = new Array[Byte](32)
    secondDigest.doFinal(result, 0)
    result
  }

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
    digest.update(parent.id, 0, parent.id.length)
    @tailrec
    def generateHeader(): Header = {
      val nonce = Random.nextLong()
      val currentDigest = new Blake2bDigest(digest)
      Equihash.hashNonce(currentDigest, nonce)
      val solutions = Equihash.gbpBasic(currentDigest, n, k)
      val suitableSolution = solutions.find(solution => {
        val h = blockHash(parent.id, nonce, solution)
        correctWorkDone(h, difficulty)
      })
      suitableSolution match {
        case Some(foundSolution) =>
          Header(0.toByte, parent.id, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp, nonce,
            // todo
            foundSolution.map(_.toByte).toArray, nBits, height, votes)
        case None =>
          generateHeader()
      }
    }

    generateHeader()
  }

  private def constructInterlinkVector(parent: Header): Seq[Array[Byte]] = {
    val genesisId = if (parent.isGenesis) parent.id else parent.interlinks.head

    def generateInnerchain(curDifficulty: BigInt, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = {
      if (parent.realDifficulty >= curDifficulty) {
        generateInnerchain(curDifficulty * 2, acc :+ parent.id)
      } else {
        parent.interlinks.find(pId => Algos.blockIdDifficulty(pId) >= curDifficulty) match {
          case Some(id) if !(id sameElements genesisId) => generateInnerchain(curDifficulty * 2, acc :+ id)
          case _ => acc
        }
      }
    }

    genesisId +: generateInnerchain(Constants.InitialDifficulty * 2, Seq[Array[Byte]]())
  }

  def correctWorkDone(id: Array[Byte], difficulty: BigInt): Boolean = {
    val target = Constants.MaxTarget / difficulty
    BigInt(1, id) < target
  }

}
