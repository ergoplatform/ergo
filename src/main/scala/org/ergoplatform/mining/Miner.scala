package org.ergoplatform.mining

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Constants
import scorex.core.block.Block._

import scala.annotation.tailrec
import scala.util.Random

object Miner {


  def genBlock(difficulty: BigInt,
                parent: Header,
                stateRoot: Array[Byte],
                adProofs: ADProofs,
                transactionsRoot: Seq[AnyoneCanSpendTransaction],
                timestamp: Timestamp): ErgoFullBlock = {
???
  }

    def genHeader(difficulty: BigInt,
                parent: Header,
                stateRoot: Array[Byte],
                adProofsRoot: Array[Byte],
                transactionsRoot: Array[Byte],
                timestamp: Timestamp): Header = {
//    val interlinks: Seq[Array[Byte]] = if (parent.isGenesis) constructInterlinkVector(parent)
//    else Seq(parent.id)
    val interlinksRoot: Array[Byte] = ???

    @tailrec
    def generateHeader(): Header = {
      val nonce = Random.nextInt
      val header = Header(0.toByte, parent.id, interlinksRoot, adProofsRoot, stateRoot, transactionsRoot, timestamp, nonce)
      if (correctWorkDone(header.id, difficulty)) header
      else generateHeader()
    }
    generateHeader()
  }

  private def blockIdDifficulty(id: Array[Version]): BigInt = {
    val blockTarget = BigInt(1, id)
    Constants.MaxTarget / blockTarget
  }

/*
  private def constructInterlinkVector(parent: Header): Seq[Array[Byte]] = {
    val genesisId = if (parent.isGenesis) parent.id else parent.interlinks.head

    def generateInnerchain(curDifficulty: BigInt, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = {
      if (parent.realDifficulty >= curDifficulty) {
        generateInnerchain(curDifficulty * 2, acc :+ parent.id)
      } else {
        parent.interlinks.find(pId => blockIdDifficulty(pId) >= curDifficulty) match {
          case Some(id) if !(id sameElements genesisId) => generateInnerchain(curDifficulty * 2, acc :+ id)
          case _ => acc
        }
      }
    }

    genesisId +: generateInnerchain(Constants.InitialDifficulty * 2, Seq[Array[Byte]]())
  }
*/

  def correctWorkDone(id: Array[Version], difficulty: BigInt): Boolean = {
    val target = Constants.MaxTarget / difficulty
    BigInt(1, id) < target
  }

}
