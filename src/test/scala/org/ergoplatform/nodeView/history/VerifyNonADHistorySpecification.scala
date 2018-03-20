package org.ergoplatform.nodeView.history

import java.io.File

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, HeaderChain, HeaderSerializer}
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import scorex.crypto.encode.Base58

import scala.util.Random

class VerifyNonADHistorySpecification extends HistorySpecification {

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  property("nextModifiersToDownload") {
    var history = genHistory()
    val chain = genChain(BlocksToKeep)
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.append(chain.head.blockTransactions)
    history.append(chain.head.aDProofs.get)
    history.bestFullBlockOpt.get shouldBe chain.head

    val missedChain = chain.tail.toList
    val missedBT = missedChain.map(fb => (BlockTransactions.modifierTypeId, fb.blockTransactions.encodedId))
    history.nextModifiersToDownload(1, Seq()).map(id => (id._1, Base58.encode(id._2))) shouldEqual missedBT.take(1)
    history.nextModifiersToDownload(BlocksToKeep - 1, Seq()).map(id => (id._1, Base58.encode(id._2))) shouldEqual missedBT

    history.nextModifiersToDownload(2, Seq(missedChain.head.blockTransactions.id)).map(id => (id._1, Base58.encode(id._2))) shouldEqual missedBT.tail.take(2)
  }

  property("append header as genesis") {
    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = genHeaderChain(1, history).head
    val updHistory = history.append(header).get._1
    updHistory.bestHeaderOpt shouldBe Some(header)
    updHistory.modifierById(header.id) shouldBe Some(header)
  }

  property("append header as genesis - via applyHeaderChain") {
    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = genHeaderChain(1, history).head

    val updHistory = applyHeaderChain(history, HeaderChain(Seq(header)))
    updHistory.bestHeaderOpt shouldBe Some(header)
    updHistory.modifierById(header.id) shouldBe Some(header)
  }

  property("append header to genesis - 2") {
    val (us, bh) = ErgoState.generateGenesisUtxoState(new File(s"/tmp/ergo/${Random.nextInt()}").ensuring(_.mkdirs()), None)

    val block = validFullBlock(None, us, bh)

    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = block.header

    HeaderSerializer.parseBytes(HeaderSerializer.toBytes(header)).get shouldBe header

    val actualHeader = history.append(header).get._1.bestHeaderOpt.get
    actualHeader shouldBe header
  }

  property("Appended headers and transactions blocks to best chain in tx history") {
    var history = genHistory()

    history = applyChain(history, genChain(BlocksInChain, history))

    genChain(BlocksInChain, history).tail.foreach { fullBlock =>
      val startFullBlock = history.bestFullBlockOpt.get

      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      history.contains(header) shouldBe false
      history.contains(txs) shouldBe false
      history.applicable(header) shouldBe true
      history.applicable(txs) shouldBe false

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe true
      history.bestHeaderOpt.get shouldBe header

      history.bestFullBlockOpt.get shouldBe startFullBlock

      history.openSurfaceIds().head shouldBe startFullBlock.header.id

      history = history.append(txs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe false
      history.bestHeaderOpt.get shouldBe header
      history.bestFullBlockOpt.get.header shouldBe fullBlock.header
    }
  }

}
