package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HeaderChain}

import scala.util.Random

class VerifyADHistorySpecification extends HistorySpecification {

  private def genHistory() =
    generateHistory(verifyTransactions = true, ADState = true, PoPoWBootstrap = false, BlocksToKeep)

  property("bootstrap from headers and last full blocks") {
    var history = generateHistory(verifyTransactions = true, ADState = true, PoPoWBootstrap = false, BlocksToKeep)
    //todo: reconsider history.bestHeaderOpt.get shouldBe ErgoFullBlock.genesis.header
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2, Seq())

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.get shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None

    val fullBlocksToApply = chain.takeRight(BlocksToKeep)

    history = history.append(fullBlocksToApply.head.blockTransactions).get._1
    history.bestFullBlockOpt shouldBe None
    history = history.append(fullBlocksToApply.head.aDProofs.get).get._1
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.head.header
  }

  property("syncInfo()") {
    var history = genHistory()

    val chain = genChain(BlocksInChain, bestFullOptToSeq(history))
    val answer = Random.nextBoolean()
    history = applyChain(history, chain)
    val si = history.syncInfo(answer)
    si.answer shouldBe answer
    si.lastHeaderIds.last shouldEqual chain.last.header.id
    si.fullBlockIdOpt.get shouldEqual history.bestFullBlockIdOpt.get
  }

  property("reportSemanticValidity should change nothing for valid blocks") {
    var history = genHistory()

    genChain(BlocksInChain, bestFullOptToSeq(history)).tail.foreach { fullBlock =>
      history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
      history.bestHeaderOpt.get.id shouldEqual fullBlock.parentId

      history = history.append(fullBlock.header).get._1.append(fullBlock.aDProofs.get).get._1
        .append(fullBlock.blockTransactions).get._1

      history.reportSemanticValidity(fullBlock.header, valid = true, fullBlock.header.parentId)
      history.reportSemanticValidity(fullBlock.aDProofs.get, valid = true, fullBlock.header.parentId)
      history.reportSemanticValidity(fullBlock.blockTransactions, valid = true, fullBlock.header.parentId)

      history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
      history.bestHeaderOpt.get.id shouldEqual fullBlock.id
      //TODO check that modifier validity is persisted with isSemanticallyValid?
    }
  }


  property("Report invalid for best full block") {
    var history = genHistory()

    val chain = genChain(BlocksInChain, bestFullOptToSeq(history))
    history = applyChain(history, chain)

    chain.takeRight(BlocksToKeep - 2).reverse.foreach { fullBlock =>
      history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
      history.bestHeaderOpt.get shouldEqual fullBlock.header

      val parentHeader = history.typedModifierById[Header](fullBlock.header.parentId).get
      history.contains(parentHeader.transactionsId) shouldBe true
      history.contains(parentHeader.ADProofsId) shouldBe true

      //todo: why parentHeader.id?
      val (repHistory, _) = history.reportSemanticValidity(fullBlock.blockTransactions, false, parentHeader.id)
      repHistory.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
      repHistory.bestHeaderOpt.get shouldBe parentHeader
    }
  }

  property("continuationIds() should contain ids of adProofs and blockTransactions") {
    var history = genHistory()

    val chain = genChain(BlocksInChain, bestFullOptToSeq(history))

    history = applyChain(history, chain)
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 1) {
        val theirBestFull = Some(chain(chain.size - forkLength).header.id)
        val si = ErgoSyncInfo(answer = true, chain.map(_.header.id), theirBestFull)
        val continuation = history.continuationIds(si, forkLength).get

        continuation.count(_._1 == ADProofs.modifierTypeId) shouldBe forkLength - 1
        continuation.count(_._1 == BlockTransactions.modifierTypeId) shouldBe forkLength - 1

      }
    }
  }

  property("prune old blocks test") {
    val blocksToPrune = 20

    var history = genHistory()
    val chain = genChain(BlocksToKeep + blocksToPrune + 1, Seq())

    history = applyChain(history, chain)
    history.bestHeaderOpt.get shouldBe chain.last.header
    history.bestFullBlockOpt.get.header shouldBe chain.last.header

    //genesis block is not pruned
    chain.take(blocksToPrune).tail.foreach { b =>
      history.modifierById(b.header.transactionsId) shouldBe None
      history.modifierById(b.header.ADProofsId) shouldBe None
    }

    chain.takeRight(BlocksToKeep).foreach { b =>
      history.modifierById(b.header.transactionsId).isDefined shouldBe true
      history.modifierById(b.header.ADProofsId).isDefined shouldBe true
    }
  }

  property("process fork") {
    var history = applyChain(genHistory(), genChain(BlocksInChain, Seq()))

    assert(history.bestFullBlockOpt.isDefined)
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 0) {
        val fork1 = genChain(forkLength, Seq(history.bestFullBlockOpt.get)).tail
        val fork2 = genChain(forkLength + 1, Seq(history.bestFullBlockOpt.get)).tail

        history = applyChain(history, fork1)
        history.bestHeaderOpt.get shouldBe fork1.last.header

        history = applyChain(history, fork2)
        history.bestHeaderOpt.get shouldBe fork2.last.header
      }
    }
  }

  property("Appended full blocks to best chain in full history") {
    var history = applyChain(genHistory(), genChain(BlocksInChain, Seq()))

    val chain = genChain(BlocksInChain, bestFullOptToSeq(history)).tail
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
      history.bestHeaderOpt.get shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock
      history.openSurfaceIds().head shouldEqual startFullBlock.header.id

      history = history.append(txs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(proofs) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe true
      history.applicable(txs) shouldBe false
      history.bestHeaderOpt.get shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock

      history = history.append(proofs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(proofs) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe false
      history.applicable(txs) shouldBe false
      history.bestHeaderOpt.get shouldBe header
      history.bestFullBlockOpt.get shouldBe fullBlock
      history.openSurfaceIds().head shouldEqual fullBlock.header.id
    }
  }
}
