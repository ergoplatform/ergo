package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HeaderChain}
import org.ergoplatform.nodeView.state.StateType
import scorex.core.consensus.{Absent, Invalid, Unknown, Valid}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Random, Try}


class VerifyADHistorySpecification extends HistorySpecification {

  private def genHistory(height: Int = 0, minimalFullBlockHeight: Int = 0): ErgoHistory = {
    val inHistory = generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, BlocksToKeep)
    inHistory.minimalFullBlockHeight = minimalFullBlockHeight
    if (height > 0) applyChain(inHistory, genChain(height, inHistory))
    else inHistory
  }

  property("proofs and transactions application in random order with forks") {
    forAll(smallInt, positiveLongGen) { (chainHeight, seed) =>
      whenever(chainHeight > 0) {
        var history = genHistory()
        val r = new Random(seed)
        val genesis = genChain(1).head
        history.append(genesis.header) shouldBe 'success
        history.append(genesis.blockTransactions) shouldBe 'success
        history.append(genesis.aDProofs.get) shouldBe 'success
        history.bestFullBlockOpt shouldBe Some(genesis)

        val chains = Seq(genChain(chainHeight, genesis), genChain(chainHeight + 1, genesis)).map(_.tail)
        chains.foreach(chain => applyHeaderChain(history, HeaderChain(chain.map(_.header))))
        val indices: Seq[(Int, Int)] = chains.indices.flatMap { chainIndex =>
          val chain = chains(chainIndex)
          chain.indices.map(blockIndex => (chainIndex, blockIndex))
        }

        var appended: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty

        def findBestBlock(appendedToCheck: Seq[ErgoFullBlock]): ErgoFullBlock = {
          def firstInAppended(h: Header): Header = {
            appended.find(_.header.id sameElements h.parentId).map(_.header) match {
              case Some(prev) => firstInAppended(prev)
              case None => h
            }
          }

          if (appendedToCheck.isEmpty) {
            genesis
          } else {
            val best = appendedToCheck.maxBy(_.header.height)
            if (firstInAppended(best.header).parentId sameElements genesis.id) {
              best
            } else {
              findBestBlock(appendedToCheck.filterNot(_.id sameElements best.id))
            }
          }
        }

        r.shuffle(indices).foreach { i =>
          val block = chains(i._1)(i._2)
          history.append(block.blockTransactions) shouldBe 'success
          history.append(block.aDProofs.get) shouldBe 'success

          appended += block

          findBestBlock(appended).header.height shouldBe history.bestFullBlockOpt.get.header.height
        }
      }
    }
  }

  property("apply proofs that link incomplete chain") {
    var history = genHistory()
    val chain = genChain(4)

    val block0 = chain.head
    val block1 = chain(1)
    val block2 = chain(2)
    val block3 = chain(3)

    chain.foreach(f => history.append(f.header))
    history.bestFullBlockOpt shouldBe None
    history.bestHeaderOpt shouldBe Some(block3.header)

    history.append(block0.blockTransactions) shouldBe 'success
    history.append(block0.aDProofs.get) shouldBe 'success
    history.bestFullBlockOpt shouldBe Some(block0)

    history.append(block2.blockTransactions) shouldBe 'success
    history.append(block2.aDProofs.get) shouldBe 'success
    history.bestFullBlockOpt shouldBe Some(block0)

    history.append(block3.blockTransactions) shouldBe 'success
    history.append(block3.aDProofs.get) shouldBe 'success
    history.bestFullBlockOpt shouldBe Some(block0)

    history.append(block1.blockTransactions) shouldBe 'success
    history.append(block1.aDProofs.get) shouldBe 'success
    history.bestFullBlockOpt shouldBe Some(block3)
  }

  //TODO fix this correctly
  ignore("bootstrap from headers and last full blocks") {
    var history = genHistory()
    //todo: reconsider history.bestHeaderOpt.get shouldBe ErgoFullBlock.genesis.header
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2)

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

    val chain = genChain(BlocksInChain, history)
    history = applyChain(history, chain)
    val si = history.syncInfo
    si.lastHeaderIds.last shouldEqual chain.last.header.id
  }

  property("reportSemanticValidity(valid = true) when better header chain exists") {
    var history = genHistory()

    val inChain = genChain(2, history)
    history = applyChain(history, inChain)

    val fork1 = genChain(3, history).tail
    val fork2 = genChain(4, history).tail
    fork1.head.parentId shouldEqual fork2.head.parentId

    history = applyChain(history, fork1)
    history = applyHeaderChain(history, HeaderChain(fork2.map(_.header)))
    history.bestFullBlockOpt.get shouldBe fork1.last
    history.bestHeaderOpt.get shouldBe fork2.last.header

    fork1.indices.foreach { i =>
      val fullBlock = fork1(i)
      val progressInfo = history.reportSemanticValidity(fullBlock, valid = true, fullBlock.header.parentId)
      val nextBlock = Try(fork1(i + 1)).toOption
      progressInfo._2.toApply shouldBe nextBlock
    }
  }

  property("reportSemanticValidity(valid = true) should set isSemanticallyValid() result") {
    var history = genHistory()

    val chain = genChain(BlocksInChain, history)
    chain.foreach { fullBlock =>
      history.bestHeaderOpt.foreach(b => b.id shouldEqual fullBlock.parentId)
      history.bestFullBlockOpt.foreach(b => b.header shouldBe history.bestHeaderOpt.get)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Absent
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe Absent
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Absent

      history = history.append(fullBlock.header).get._1
      history = history.append(fullBlock.aDProofs.get).get._1
      history = history.append(fullBlock.blockTransactions).get._1

      history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
      history.bestHeaderOpt.get.id shouldEqual fullBlock.header.id

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Unknown
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe Unknown
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Unknown

      history.reportSemanticValidity(fullBlock.header, valid = true, fullBlock.header.parentId)
      history.reportSemanticValidity(fullBlock.aDProofs.get, valid = true, fullBlock.header.parentId)
      history.reportSemanticValidity(fullBlock.blockTransactions, valid = true, fullBlock.header.parentId)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Valid
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe Valid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Valid
    }
  }

  property("reportSemanticValidity(valid = false) should set isSemanticallyValid() result for all linked modifiers") {
    var history = genHistory(1)

    history.bestFullBlockOpt should not be None

    val chain = genChain(BlocksInChain, history.bestFullBlockOpt.get).tail
    (chain.head.header.parentId sameElements Header.GenesisParentId) shouldBe false

    history = applyChain(history, chain)

    chain.reverse.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Unknown
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe Unknown
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Unknown

      history.reportSemanticValidity(fullBlock.header, valid = false, fullBlock.header.parentId)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }
  }

  property("reportSemanticValidity(valid = false) should mark invalid all forks containing this header") {
    var history = genHistory()

    val inChain = genChain(2, history)
    history = applyChain(history, inChain)

    val fork1 = genChain(3, history).tail
    val fork2 = genChain(3, history).tail
    fork1.head.parentId shouldEqual fork2.head.parentId

    history = applyChain(history, fork1)
    history = applyChain(history, fork2)

    history.reportSemanticValidity(inChain.last.header, valid = false, inChain.last.parentId)

    fork1.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }

    fork2.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }
  }

  property("reportSemanticValidity(valid = false) should return blocks to rollback and to process") {
    var history = genHistory(3)
    val common = history.bestFullBlockOpt.get

    val fork1 = genChain(3, common).tail
    val fork2 = genChain(2, common).tail

    history = applyChain(history, fork1)
    history = applyChain(history, fork2)

    history.bestHeaderOpt.get shouldBe fork1.last.header

    val res = history.reportSemanticValidity(fork1.head.header, valid = false, common.parentId)

    history.bestHeaderOpt.get shouldBe fork2.last.header
    history.bestFullBlockOpt.get shouldBe fork2.last
  }

  property("reportSemanticValidity(valid = false) for non-last block in best chain without better forks") {
    var history = genHistory()
    val chain = genChain(BlocksInChain, history)
    history = applyChain(history, chain)

    history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
    history.bestHeaderOpt.get shouldEqual chain.last.header

    val invalidChain = chain.takeRight(2)

    val report = history.reportSemanticValidity(invalidChain.head.header, valid = false, invalidChain.head.header.id)
    history = report._1
    val processInfo = report._2
    processInfo.toApply.isEmpty shouldBe true
    processInfo.branchPoint.get shouldEqual invalidChain.head.header.parentId
    processInfo.toRemove shouldEqual invalidChain

    history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
    history.bestHeaderOpt.get.id shouldEqual invalidChain.head.parentId
  }

  property("Report invalid for best full block") {
    var history = genHistory()

    val chain = genChain(BlocksInChain, history)
    history = applyChain(history, chain)

    chain.takeRight(BlocksToKeep - 2).reverse.foreach { fullBlock =>
      history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
      history.bestHeaderOpt.get shouldEqual fullBlock.header

      val parentHeader = history.typedModifierById[Header](fullBlock.header.parentId).get
      history.contains(parentHeader.transactionsId) shouldBe true
      history.contains(parentHeader.ADProofsId) shouldBe true

      //todo: why parentHeader.id?
      val (repHistory, _) = history.reportSemanticValidity(fullBlock.blockTransactions, valid = false, parentHeader.id)
      repHistory.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
      repHistory.bestHeaderOpt.get shouldBe parentHeader
    }
  }

  property("prune old blocks test") {
    val blocksToPrune = 20

    var history = genHistory()
    val chain = genChain(BlocksToKeep + blocksToPrune + 1)

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

  property("process fork from genesis") {
    val genesis = genChain(1).head
    var history = applyChain(genHistory(), Seq(genesis))
    val fork1 = genChain(1, history.bestFullBlockOpt.get).tail
    val fork2 = genChain(2, history.bestFullBlockOpt.get).tail

    history = applyChain(history, fork1)
    history.bestHeaderOpt.get shouldBe fork1.last.header

    history = applyChain(history, fork2.dropRight(1))
    val lastBlock = fork2.last
    history = history.append(lastBlock.header).get._1.append(lastBlock.blockTransactions).get._1

    val changes = history.append(lastBlock.aDProofs.get).get
    history = changes._1
    history.bestHeaderOpt.get shouldBe fork2.last.header

    val processInfo = changes._2
    processInfo.branchPoint.get shouldEqual genesis.id
    processInfo.toRemove should contain theSameElementsAs fork1
    processInfo.toApply shouldBe fork2.headOption

  }

  property("process fork from existing chain") {
    var history = applyChain(genHistory(), genChain(BlocksInChain))

    history.bestFullBlockOpt.isDefined should not be None
    forAll(smallPositiveInt) { forkLength: Int =>
      whenever(forkLength > 0) {
        val branchPoint = history.bestFullBlockOpt.get
        val fork1 = genChain(forkLength, branchPoint).tail
        val fork2 = genChain(forkLength + 1, branchPoint).tail

        history = applyChain(history, fork1)
        history.bestHeaderOpt.get shouldBe fork1.last.header

        history = applyChain(history, fork2.dropRight(1))
        val lastBlock = fork2.last
        history = history.append(lastBlock.header).get._1.append(lastBlock.blockTransactions).get._1

        val changes = history.append(lastBlock.aDProofs.get).get
        history = changes._1
        history.bestHeaderOpt.get shouldBe fork2.last.header

        val processInfo = changes._2
        processInfo.branchPoint.get shouldEqual branchPoint.id
        processInfo.toRemove should contain theSameElementsAs fork1
        processInfo.toApply shouldBe fork2.headOption

      }
    }
  }

  property("Appended full blocks to best chain in full history") {
    var history = genHistory(1)
    history.bestFullBlockOpt.nonEmpty shouldBe true

    val chain = genChain(BlocksInChain, history).tail
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
