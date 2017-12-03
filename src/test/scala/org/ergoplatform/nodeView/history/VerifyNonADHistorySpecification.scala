package org.ergoplatform.nodeView.history

import java.io.File

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, HeaderChain, HeaderSerializer}
import org.ergoplatform.nodeView.state.ErgoState

import scala.util.Random

class VerifyNonADHistorySpecification extends HistorySpecification {

  private def genHistory() =
    generateHistory(verifyTransactions = true, ADState = false, PoPoWBootstrap = false, BlocksToKeep)

  property("missedModifiersForFullChain") {
    var history = genHistory()
    val chain = genChain(BlocksToKeep, Seq())
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))

    val missed = history.missedModifiersForFullChain()
    missed.filter(_._1 == BlockTransactions.modifierTypeId).map(_._2) should contain theSameElementsAs chain.map(_.blockTransactions.id)
    missed.filter(_._1 == ADProofs.modifierTypeId).map(_._2) should contain theSameElementsAs chain.map(_.aDProofs.get.id)
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
    val (us, bh) = ErgoState.generateGenesisUtxoState(new File(s"/tmp/ergo/${Random.nextInt()}").ensuring(_.mkdirs()))

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

    history = applyChain(history, genChain(BlocksInChain, bestFullOptToSeq(history)))

    genChain(BlocksInChain, bestFullOptToSeq(history)).tail.foreach { fullBlock =>
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
