package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HeaderChain}
import scorex.core.consensus.ModifierSemanticValidity

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class VerifyADHistorySpecification extends HistorySpecification {

  private def genHistory(height: Int = 0): ErgoHistory = {
    val inHistory = generateHistory(verifyTransactions = true, ADState = true, PoPoWBootstrap = false, BlocksToKeep)
    if (height > 0) applyChain(inHistory, genChain(height, bestFullOptToSeq(inHistory)))
    else inHistory
  }

  property("missedModifiersForFullChain") {
    var history = genHistory()
    val chain = genChain(BlocksToKeep, Seq())
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))

    val missed = history.missedModifiersForFullChain()
    missed.filter(_._1 == BlockTransactions.modifierTypeId).map(_._2) should contain theSameElementsAs chain.map(_.blockTransactions.id)
    missed.filter(_._1 == ADProofs.modifierTypeId).map(_._2) should contain theSameElementsAs chain.map(_.aDProofs.get.id)
  }

  property("apply proofs and transactions in random order") {
    forAll(smallInt, positiveLongGen) { (chainHeight, seed) =>
      whenever(chainHeight > 0 && chainHeight <= 20) {
        var history = generateHistory(verifyTransactions = true, ADState = true, PoPoWBootstrap = false, BlocksToKeep)
        val r = new Random(seed)
        val chain = genChain(chainHeight, Seq())
        history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))

        r.shuffle(chain.indices.toList).foreach { i =>
          val prevBest = history.bestFullBlockOpt
          val block = chain(i)
          history.append(block.blockTransactions) shouldBe 'success
          history.append(block.aDProofs.get) shouldBe 'success

          prevBest match {
            case None if block.header.isGenesis =>
              history.bestFullBlockOpt should not be prevBest
            case Some(p) if p.id sameElements block.parentId =>
              history.bestFullBlockOpt should not be prevBest
            case _ =>
              history.bestFullBlockOpt shouldBe prevBest
          }
        }
        history.bestFullBlockOpt shouldBe chain.lastOption
      }
    }
  }

  property("fork processing at proofs and transactions application in random order") {
    forAll(smallInt, positiveLongGen) { (chainHeight, seed) =>
      whenever(chainHeight > 0) {
        var history = generateHistory(verifyTransactions = true, ADState = true, PoPoWBootstrap = false, BlocksToKeep)
        val r = new Random(seed)
        val genesis = genChain(1, Seq()).head
        history.append(genesis.header) shouldBe 'success
        history.append(genesis.blockTransactions) shouldBe 'success
        history.append(genesis.aDProofs.get) shouldBe 'success
        history.bestFullBlockOpt shouldBe Some(genesis)

        val chains = Seq(genChain(chainHeight, Seq(genesis)), genChain(chainHeight + 1, Seq(genesis))).map(_.tail)
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
              findBestBlock(appendedToCheck.filter(b => !(b.id sameElements best.id)))
            }
          }
        }

        r.shuffle(indices).foreach { i =>
          val block = chains(i._1)(i._2)
          history.append(block.blockTransactions) shouldBe 'success
          history.append(block.aDProofs.get).get

          appended += block

          findBestBlock(appended) shouldBe history.bestFullBlockOpt.get
        }
      }
    }
  }


  property("apply proofs that link incomplete chain") {
    var history = generateHistory(verifyTransactions = true, ADState = true, PoPoWBootstrap = false, BlocksToKeep)
    val chain = genChain(4, Seq())

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
    history = applyChain(history, chain)
    val si = history.syncInfo
    si.lastHeaderIds.last shouldEqual chain.last.header.id
  }

  property("reportSemanticValidity(valid = true) should set isSemanticallyValid() result") {
    var history = genHistory()

    val chain = genChain(BlocksInChain, bestFullOptToSeq(history))
    chain.foreach { fullBlock =>
      history.bestHeaderOpt.foreach(b => b.id shouldEqual fullBlock.parentId)
      history.bestFullBlockOpt.foreach(b => b.header shouldBe history.bestHeaderOpt.get)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe ModifierSemanticValidity.Absent
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe ModifierSemanticValidity.Absent
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Absent

      history = history.append(fullBlock.header).get._1
      history = history.append(fullBlock.aDProofs.get).get._1
      history = history.append(fullBlock.blockTransactions).get._1

      history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
      history.bestHeaderOpt.get.id shouldEqual fullBlock.header.id

      history.isSemanticallyValid(fullBlock.header.id) shouldBe ModifierSemanticValidity.Unknown
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe ModifierSemanticValidity.Unknown
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Unknown

      history.reportSemanticValidity(fullBlock.header, valid = true, fullBlock.header.parentId)
      history.reportSemanticValidity(fullBlock.aDProofs.get, valid = true, fullBlock.header.parentId)
      history.reportSemanticValidity(fullBlock.blockTransactions, valid = true, fullBlock.header.parentId)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe ModifierSemanticValidity.Valid
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe ModifierSemanticValidity.Valid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Valid
    }

  }

  property("reportSemanticValidity(valid = false) should set isSemanticallyValid() result for all linked modifiers") {
    var history = genHistory(1)

    history.bestFullBlockOpt should not be None

    val chain = genChain(BlocksInChain, Seq(history.bestFullBlockOpt.get)).tail
    (chain.head.header.parentId sameElements Header.GenesisParentId) shouldBe false

    history = applyChain(history, chain)

    chain.reverse.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe ModifierSemanticValidity.Unknown
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe ModifierSemanticValidity.Unknown
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Unknown

      history.reportSemanticValidity(fullBlock.header, valid = false, fullBlock.header.parentId)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe ModifierSemanticValidity.Invalid
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe ModifierSemanticValidity.Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Invalid
    }
  }

  property("reportSemanticValidity(valid = false) should mark invalid all forks containing this header") {
    var history = genHistory()

    val inChain = genChain(2, bestFullOptToSeq(history))
    history = applyChain(history, inChain)

    val fork1 = genChain(3, bestFullOptToSeq(history)).tail
    history = applyChain(history, fork1)
    val fork2 = genChain(3, bestFullOptToSeq(history)).tail
    history = applyChain(history, fork2)

    history.reportSemanticValidity(inChain.last.header, valid = false, inChain.last.parentId)

    fork1.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe ModifierSemanticValidity.Invalid
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe ModifierSemanticValidity.Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Invalid
    }

    fork2.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe ModifierSemanticValidity.Invalid
      history.isSemanticallyValid(fullBlock.aDProofs.get.id) shouldBe ModifierSemanticValidity.Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Invalid
    }
  }

  property("reportSemanticValidity(valid = false) should return blocks to rollback and to process") {
    var history = genHistory(3)
    val common = history.bestFullBlockOpt.get

    val fork1 = genChain(3, Seq(common)).tail
    val fork2 = genChain(2, Seq(common)).tail
    history = applyChain(history, fork1)
    history = applyChain(history, fork2)

    history.bestHeaderOpt.get shouldBe fork1.last.header

    val res = history.reportSemanticValidity(fork1.head.header, valid = false, common.parentId)

    history.bestHeaderOpt.get shouldBe fork2.last.header
    history.bestFullBlockOpt.get shouldBe fork2.last
  }

  property("reportSemanticValidity(valid = false) for non-last block in best chain without better forks") {
    var history = genHistory()
    val chain = genChain(BlocksInChain, bestFullOptToSeq(history))
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

    val chain = genChain(BlocksInChain, bestFullOptToSeq(history))
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

  property("process fork from genesis") {
    val genesis = genChain(1, Seq()).head
    var history = applyChain(genHistory(), Seq(genesis))
    val fork1 = genChain(1, Seq(history.bestFullBlockOpt.get)).tail
    val fork2 = genChain(2, Seq(history.bestFullBlockOpt.get)).tail

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
    var history = applyChain(genHistory(), genChain(BlocksInChain, Seq()))

    history.bestFullBlockOpt.isDefined should not be None
    forAll(smallPositiveInt) { forkLength: Int =>
      whenever(forkLength > 0) {
        val branchPoint = history.bestFullBlockOpt.get
        val fork1 = genChain(forkLength, Seq(branchPoint)).tail
        val fork2 = genChain(forkLength + 1, Seq(branchPoint)).tail

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
