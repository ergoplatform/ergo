package org.ergoplatform.nodeView.history

class VerifyNonADHistorySpecification extends HistorySpecification {

  var history = generateHistory(verify = true, adState = false, BlocksToKeep)

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
