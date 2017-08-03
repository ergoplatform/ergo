package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import scorex.core.consensus.History.HistoryComparisonResult

class VerifyNonADHistorySpecification extends HistorySpecification {

  var history = generateHistory(verify = true, adState = false, BlocksToKeep)

  property("compare() for full chain") {
    def getInfo(c: Seq[ErgoFullBlock]) = ErgoSyncInfo(answer = true, c.map(_.header.id), Some(c.last.header.id))

    val fork1 = genChain(BlocksInChain, Seq(history.bestFullBlockOpt.get))
    val fork2 = genChain(BlocksInChain + 1, Seq(history.bestFullBlockOpt.get))

    history = applyChain(history, fork1.tail)
    history.bestHeader shouldBe fork1.last.header

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
    assert(history.bestFullBlockOpt.get.header == history.bestHeader)
    val chain = genChain(BlocksInChain, Seq(history.bestFullBlockOpt.get)).tail
    chain.foreach { fullBlock =>
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
      history.bestHeader shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock
      history.openSurfaceIds().head shouldEqual startFullBlock.header.id

      history = history.append(txs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe false
      history.bestHeader shouldBe header
      history.bestFullBlockOpt.get.header shouldBe fullBlock.header
    }
  }

}
