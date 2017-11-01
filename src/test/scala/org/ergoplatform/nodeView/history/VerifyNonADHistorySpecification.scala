package org.ergoplatform.nodeView.history

import java.io.File

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{HeaderChain, HeaderSerializer}
import org.ergoplatform.nodeView.state.ErgoState
import scorex.core.consensus.History.HistoryComparisonResult

import scala.util.Random

class VerifyNonADHistorySpecification extends HistorySpecification {

  private def genHistory() =
    generateHistory(verifyTransactions = true, ADState = false, PoPoWBootstrap = false, BlocksToKeep)

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
    val tmp = new File(s"/tmp/ergo/${Random.nextInt()}")
    require(tmp.mkdirs())
    val (us, bh) = ErgoState.generateGenesisUtxoState(tmp)

    val block = validFullBlock(None, us, bh)

    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = block.header

    HeaderSerializer.parseBytes(HeaderSerializer.toBytes(header)).get shouldBe header

    val actualHeader = history.append(header).get._1.bestHeaderOpt.get
    actualHeader shouldBe header
  }

  property("compare() for full chain") {
    def getInfo(c: Seq[ErgoFullBlock]) = ErgoSyncInfo(answer = true, c.map(_.header.id), Some(c.last.header.id))

    var history = genHistory()

    history = applyChain(history, genChain(BlocksInChain, bestFullOptToSeq(history)))

    val fork1 = genChain(BlocksInChain, bestFullOptToSeq(history))
    val fork2 = genChain(BlocksInChain + 1, bestFullOptToSeq(history))

    history = applyChain(history, fork1.tail)
    history.bestHeaderOpt.get shouldBe fork1.last.header

    //Compare different headers chain
    history.compare(getInfo(fork2)) shouldBe HistoryComparisonResult.Older
    history.compare(getInfo(fork1)) shouldBe HistoryComparisonResult.Equal
    history.compare(getInfo(fork1.take(BlocksInChain - 1))) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork2.take(BlocksInChain - 1))) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork2.tail)) shouldBe HistoryComparisonResult.Nonsense

    //Equals Header chain, different full chain
    history.compare(getInfo(fork1).copy(fullBlockIdOpt = None)) shouldBe HistoryComparisonResult.Equal
    val worstFullBlock = getInfo(fork1).copy(fullBlockIdOpt = Some(fork1(fork1.length - 2).header.id))
    history.compare(worstFullBlock) shouldBe HistoryComparisonResult.Younger
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
