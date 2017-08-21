package org.ergoplatform.mining

import org.bouncycastle.crypto.digests.{Blake2bDigest, SHA256Digest}
import org.ergoplatform.crypto.Equihash
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
    val h = new Array[Byte](16)
    digest.doFinal(h, 0)
    val secondDigest = new SHA256Digest()
    secondDigest.update(h, 0, h.length)
    val result = new Array[Byte](16)
    secondDigest.doFinal(result, 0)
    result
  }

  def difficultyFilter(prevHash: Array[Byte], nonce: BigInt, soln: Seq[Int], d: BigInt): Boolean = {
    val h = blockHash(prevHash, nonce, soln)
    val count = Equihash.countLeadingZeroes(h.map(_ & 0xFF))
    count >= d
  }

  private def ergoPerson(n: Int, k: Int): Array[Byte] = "ERGOPoW".getBytes ++ leIntToByteArray(n) ++ leIntToByteArray(k)

  def genBlock(difficulty: BigInt,
               parent: Header,
               stateRoot: Array[Byte],
               adProofs: ADProof,
               transactions: Seq[AnyoneCanSpendTransaction],
               timestamp: Timestamp,
               n: Int, k: Int): ErgoFullBlock = {
    val bytesPerWord = n / 8
    val wordsPerHash = 512 / n

    val digest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, ergoPerson(n, k))
    digest.update(parent.id, 0, parent.id.length)
    val solutionOpt = (BigInt(0) until BigInt("2923003274661805836407369665432566039311865085952")).view.map(nonce => {
      val currentDigest = new Blake2bDigest(digest)
      Equihash.hashNonce(currentDigest, nonce)
      val solutions = Equihash.gbpBasic(currentDigest, n, k)
      val suitableSolution = solutions.find(solutions => difficultyFilter(parent.id, nonce, solutions, difficulty))
      suitableSolution
    }).find(_.isDefined).map(_.get)
    solutionOpt match {
      case Some(solution) =>
        val h = genHeader(difficulty,
          parent, stateRoot, adProofs.proofBytes, ???,
          // todo omg
          solution.map(_.toByte).toArray, ???, ???, ???
        )
        new ErgoFullBlock(h, BlockTransactions(h.id, transactions), None, None)
      case None =>
        throw new IllegalStateException("Solution wasn't found!")
    }
  }

  def genHeader(difficulty: BigInt,
                parent: Header,
                stateRoot: Array[Byte],
                adProofsRoot: Array[Byte],
                transactionsRoot: Array[Byte],
                equihashSolutions: Array[Byte],
                extensionHash: Array[Byte],
                votes: Array[Byte],
                timestamp: Timestamp): Header = {
    val interlinks: Seq[Array[Byte]] = if (parent.isGenesis) constructInterlinkVector(parent) else Seq(parent.id)

    @tailrec
    def generateHeader(): Header = {
      val nonce = Random.nextInt
      val header = Header(0.toByte, parent.id, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp, nonce,
        difficulty, equihashSolutions, extensionHash, votes)
      if (correctWorkDone(header.id, difficulty)) header
      else generateHeader()
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

  def correctWorkDone(id: Array[Version], difficulty: BigInt): Boolean = {
    val target = Constants.MaxTarget / difficulty
    BigInt(1, id) < target
  }

}
