package org.ergoplatform.mining

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.block.ErgoHeader
import org.ergoplatform.settings.Constants
import scorex.core.block.Block._
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.{Random, Try}

object Miner {

  def genBlock(difficulty: BigInt,
               parent: ErgoHeader,
               stateRoot: Array[Version],
               transactionsRoot: Array[Version],
               timestamp: Timestamp): ErgoHeader = {
    val interlinks: Seq[Array[Byte]] = if (parent.isGenesis) constructInterlinkVector(parent)
    else Seq(parent.id)

    @tailrec
    def generateHeader(): ErgoHeader = {
      val nonce = Random.nextInt
      val header = ErgoHeader(0.toByte, parent.id, interlinks, stateRoot, transactionsRoot, timestamp, nonce)
      if (correctWorkDone(header.id, difficulty)) header
      else generateHeader()
    }
    generateHeader()
  }

  private def blockIdDifficulty(id: Array[Version]): BigInt = {
    val blockTarget = BigInt(1, id)
    Constants.MaxTarget / blockTarget
  }

  private def constructInterlinkVector(parent: ErgoHeader): Seq[Array[Byte]] = {
    val genesisId = if(parent.isGenesis) parent.id else parent.interlinks.head

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

  def correctWorkDone(id: Array[Version], difficulty: BigInt): Boolean = {
    val target = Constants.MaxTarget / difficulty
    BigInt(1, id) < target
  }

}
