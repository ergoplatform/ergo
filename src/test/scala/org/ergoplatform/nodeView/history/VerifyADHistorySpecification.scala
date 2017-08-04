package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header, HeaderChain}

import scala.util.Random

class VerifyADHistorySpecification extends HistorySpecification {

  var history = generateHistory(verify = true, adState = true, popow = false, BlocksToKeep)

  property("bootstrap from headers and last full blocks") {
    var history = generateHistory(verify = true, adState = true, popow = false, BlocksToKeep, System.nanoTime())
    history.bestHeader shouldBe ErgoFullBlock.genesis.header
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2, Seq(ErgoFullBlock.genesis))

    history = applyHeaderChain(history, HeaderChain(chain.tail.map(_.header)))
    history.bestHeader shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None

    val fullBlocksToApply = chain.takeRight(BlocksToKeep)

    history = history.append(fullBlocksToApply.head.blockTransactions).get._1
    history.bestFullBlockOpt shouldBe None
    history = history.append(fullBlocksToApply.head.aDProofs.get).get._1
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.head.header

  }

  property("syncInfo()") {
    val chain = genChain(BlocksInChain, Seq(bestFullOrGenesis(history))).tail
    val answer = Random.nextBoolean()
    history = applyChain(history, chain)
    val si = history.syncInfo(answer)
    si.answer shouldBe answer
    si.lastHeaderIds.last shouldEqual chain.last.header.id
    si.fullBlockIdOpt.get shouldEqual history.bestFullBlockIdOpt.get
  }

  property("Report invalid for best full block") {
    assert(history.bestFullBlockOpt.get.header == history.bestHeader)
    val chain = genChain(BlocksInChain, Seq(bestFullOrGenesis(history))).tail
    history = applyChain(history, chain)

    chain.takeRight(BlocksToKeep - 2).reverse.foreach { fullBlock =>
      history.bestFullBlockOpt.get.header shouldBe history.bestHeader
      history.bestHeader shouldEqual fullBlock.header

      val parentHeader = history.typedModifierById[Header](fullBlock.header.parentId).get
      history.contains(parentHeader.transactionsId) shouldBe true
      history.contains(parentHeader.ADProofsId) shouldBe true

      history = history.reportInvalid(fullBlock.blockTransactions)
      history.bestFullBlockOpt.get.header shouldBe history.bestHeader
      history.bestHeader shouldBe parentHeader
    }
  }

  property("continuationIds() should contain ids of adProofs and blockTransactions") {
    val chain = genChain(BlocksInChain, Seq(bestFullOrGenesis(history))).tail

    history = applyChain(history, chain)
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 1) {
        val theirBestFull = Some(chain(chain.size - forkLength).header.id)
        val si = ErgoSyncInfo(answer = true, chain.map(_.header.id), theirBestFull)
        val continuation = history.continuationIds(si, forkLength).get

        continuation.count(_._1 == ADProof.ModifierTypeId) shouldBe forkLength
        continuation.count(_._1 == BlockTransactions.ModifierTypeId) shouldBe forkLength

      }
    }
  }

  property("prune old blocks test") {
    val blocksToPrune = 2
    val chain = genChain(BlocksToKeep + blocksToPrune, Seq(bestFullOrGenesis(history))).tail

    history = applyChain(history, chain)
    history.bestHeader shouldBe chain.last.header
    history.bestFullBlockOpt.get.header shouldBe chain.last.header

    chain.take(blocksToPrune).foreach { b =>
      history.modifierById(b.header.transactionsId) shouldBe None
      history.modifierById(b.header.ADProofsId) shouldBe None
    }
    chain.takeRight(BlocksToKeep).foreach { b =>
      history.modifierById(b.header.transactionsId).isDefined shouldBe true
      history.modifierById(b.header.ADProofsId).isDefined shouldBe true
    }
  }

  property("process fork") {
    assert(history.bestFullBlockOpt.isDefined)
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 0) {
        val fork1 = genChain(forkLength, Seq(history.bestFullBlockOpt.get)).tail
        val fork2 = genChain(forkLength + 1, Seq(history.bestFullBlockOpt.get)).tail

        history = applyChain(history, fork1)
        history.bestHeader shouldBe fork1.last.header

        history = applyChain(history, fork2)
        history.bestHeader shouldBe fork2.last.header
      }
    }
  }

  property("Appended full blocks to best chain in full history") {
    assert(history.bestFullBlockOpt.get.header == history.bestHeader)
    val chain = genChain(BlocksInChain, Seq(bestFullOrGenesis(history))).tail
    chain.foreach { fullBlock =>
      val startFullBlock = history.bestFullBlockOpt.get
      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      val proofs = fullBlock.aDProofs.get
      history.contains(header) shouldBe false
      history.contains(txs) shouldBe false
      history.contains(proofs) shouldBe false
      history.applicable(header) shouldBe true
      history.applicable(proofs) shouldBe false
      history.applicable(txs) shouldBe false

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe false
      history.contains(proofs) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe true
      history.applicable(txs) shouldBe true
      history.bestHeader shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock
      history.openSurfaceIds().head shouldEqual startFullBlock.header.id

      history = history.append(txs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(proofs) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe true
      history.applicable(txs) shouldBe false
      history.bestHeader shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock

      history = history.append(proofs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(proofs) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe false
      history.applicable(txs) shouldBe false
      history.bestHeader shouldBe header
      history.bestFullBlockOpt.get shouldBe fullBlock
      history.openSurfaceIds().head shouldEqual fullBlock.header.id
    }
  }
}
