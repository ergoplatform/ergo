package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{BlockTransactions, HeaderChain, HeaderSerializer}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.HistorySpecification

class VerifyNonADHistorySpecification extends HistorySpecification {

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  property("proofs and transactions application in incorrect order") {
    var history = genHistory()
    val chain = genChain(6, history)
    if(history.pruningProcessor.minimalFullBlockHeight == Int.MaxValue) {
      history.pruningProcessor.updateBestFullBlock(chain.last.header)
    }
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))

    history = history.append(chain.tail.head.blockTransactions).get._1
    history.bestFullBlockOpt shouldBe None
    val pi1 =  history.append(chain.head.blockTransactions).get._2
    history.bestFullBlockOpt.get shouldBe chain.tail.head
    pi1.toApply.length shouldBe 2

    chain.tail.tail.tail.foreach(c => history.append(c.blockTransactions))
    history.bestFullBlockOpt.get.header.height shouldBe chain.tail.head.header.height

    val pi = history.append(chain.tail.tail.head.blockTransactions).get._2
    pi.toApply.map(_.asInstanceOf[ErgoFullBlock].header.height) shouldBe Seq(2,3,4,5)
  }

  property("bootstrap from headers and last full blocks") {
    var history = genHistory()
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2)

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.get shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None

    if(history.pruningProcessor.minimalFullBlockHeight == Int.MaxValue) {
      history.pruningProcessor.updateBestFullBlock(chain.last.header)
    }

    // Until UTXO snapshot synchronization is implemented, we should always start to apply full blocks from genesis
    val fullBlocksToApply = chain

    history = history.append(fullBlocksToApply.head.blockTransactions).get._1
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.head.header
  }

  property("nextModifiersToDownload") {
    var history = genHistory()
    val chain = genChain(BlocksToKeep)
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.append(chain.head.blockTransactions)
    history.append(chain.head.aDProofs.get)
    history.bestFullBlockOpt.get shouldBe chain.head

    val missedChain = chain.tail.toList
    val missedBT = missedChain.map(fb => (BlockTransactions.modifierTypeId, fb.blockTransactions.encodedId))
    history.nextModifiersToDownload(1, Seq()).map(id => (id._1, Algos.encode(id._2))) shouldEqual missedBT.take(1)
    history.nextModifiersToDownload(BlocksToKeep - 1, Seq()).map(id => (id._1, Algos.encode(id._2))) shouldEqual missedBT

    history.nextModifiersToDownload(2, Seq(missedChain.head.blockTransactions.id)).map(id => (id._1, Algos.encode(id._2))) shouldEqual missedBT.tail.take(2)
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
    val (us, bh) = createUtxoState()

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
