package org.ergoplatform.nodeView.history

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.modifiers.history.header.{Header, PreGenesisHeader}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.nodeView.ErgoModifiersCache
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoCorePropertyTest, NoShrink}
import org.ergoplatform.utils.ErgoCoreTestConstants.defaultExtension
import org.ergoplatform.consensus.ModifierSemanticValidity._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class VerifyADHistorySpecification extends ErgoCorePropertyTest with NoShrink {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.CoreObjectGenerators.{smallInt, positiveLongGen}
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.smallPositiveInt
  import org.ergoplatform.tools.MinerBench._

  type PM = BlockSection

  private def genHistory(blocksNum: Int = 0,
                         minFullHeight: Option[Int] = Some(GenesisHeight),
                         votingLength: Option[Int] = None): (ErgoHistory, Seq[ErgoFullBlock]) = {
    val inHistory = generateHistory(
      verifyTransactions = true,
      StateType.Digest,
      PoPoWBootstrap = false,
      BlocksToKeep,
      votingLength = votingLength
    )
    minFullHeight.foreach { h =>
      inHistory.writeMinimalFullBlockHeight(h)
      inHistory.isHeadersChainSyncedVar = true
    }

    if (blocksNum > 0) {
      val chain = genChain(blocksNum, inHistory)
      (applyChain(inHistory, chain), chain)
    } else {
      (inHistory, Seq.empty)
    }
  }

  property("Forks that include genesis block") {
    var (history, _) = genHistory()
    val fork1 = genChain(3, history)
    val fork2 = genChain(2, history)
    val fork3 = genChain(4, history)

    // apply 3 headers long chain
    history = applyChain(history, fork1)
    history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
    history.bestHeaderOpt.get shouldBe fork1.last.header

    // apply 2 headers long chain, should stay on previous one
    history = applyChain(history, fork2)
    history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
    history.bestHeaderOpt.get shouldBe fork1.last.header

    // apply 4 headers long chain, should update chain
    history = applyChain(history, fork3)
    history.bestHeaderOpt.get shouldBe fork3.last.header
    history.bestFullBlockOpt.get.header.height shouldBe fork3.last.header.height

  }


  property("ErgoModifiersCache.findCandidateKey() should find headers in case of forks") {
    val modifiersCache = new ErgoModifiersCache(Int.MaxValue)

    var (history, _) = genHistory(2)

    val fork1 = genChain(2, history).tail
    val fork2 = genChain(3, history).tail

    history = applyChain(history, fork1)

    fork2.foreach { fb =>
      modifiersCache.put(fb.header.id, fb.header)
    }
    history.applicable(fork2.head.header) shouldBe true
    modifiersCache.contains(fork2.head.header.id) shouldBe true
    modifiersCache.findCandidateKey(history).isDefined shouldBe true
  }

  property("should not be able to apply blocks older than blocksToKeep") {
    var history = genHistory()._1
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2)

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.get shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None

    val fullBlocksToApply = chain.tail
    history.updateBestFullBlock(fullBlocksToApply(BlocksToKeep - 1).header)

    history.applicable(chain.head.blockTransactions) shouldBe false

    history = applyBlock(history, fullBlocksToApply.head)
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.head.header

    history.applicable(chain.head.blockTransactions) shouldBe false

    fullBlocksToApply.tail.foreach { f =>
      history = applyBlock(history, f)
    }
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.last.header

    //block sections should be already pruned
    fullBlocksToApply.head.header.sectionIds.foreach(id => history.contains(id._2) shouldBe false)

    //block transactions should not be able to apply since they are too far back in history
    fullBlocksToApply.head.blockSections.foreach(s => history.applicable(s) shouldBe false)
  }

  property("proofs and transactions application in random order with forks") {
    forAll(smallInt, positiveLongGen) { (chainHeight, seed) =>
      whenever(chainHeight > 0) {
        val (history, chain) = genHistory(1)
        val r = new Random(seed)
        val genesis = chain.head
        history.bestFullBlockOpt shouldBe Some(genesis)

        val chains = Seq(genChain(chainHeight, genesis), genChain(chainHeight + 1, genesis)).map(_.tail)
        chains.foreach(chain => applyHeaderChain(history, HeaderChain(chain.map(_.header))))
        val indices: Seq[(Int, Int)] = chains.indices.flatMap { chainIndex =>
          val chain = chains(chainIndex)
          chain.indices.map(blockIndex => (chainIndex, blockIndex))
        }

        val appended: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty

        def findBestBlock(appendedToCheck: Seq[ErgoFullBlock]): ErgoFullBlock = {
          def firstInAppended(h: Header): Header = {
            appended.find(_.header.id == h.parentId).map(_.header) match {
              case Some(prev) => firstInAppended(prev)
              case None => h
            }
          }

          if (appendedToCheck.isEmpty) {
            genesis
          } else {
            val best = appendedToCheck.maxBy(_.header.height)
            if (firstInAppended(best.header).parentId == genesis.id) {
              best
            } else {
              findBestBlock(appendedToCheck.filterNot(_.id == best.id))
            }
          }
        }

        r.shuffle(indices).foreach { i =>
          val block = chains(i._1)(i._2)
          val sectionsToAppend = block.blockSections.filterNot(_.modifierTypeId == Extension.modifierTypeId)
          r.shuffle(sectionsToAppend).foreach(s => history.append(s) shouldBe 'success)

          appended += block

          sectionsToAppend.forall(history.contains) shouldBe true
        }
      }
    }
  }

  property("apply proofs that link incomplete chain") {
    var history = genHistory()._1
    val chain = genChain(4)

    val block0 = chain.head
    val block1 = chain(1)
    val block2 = chain(2)
    val block3 = chain(3)

    applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestFullBlockOpt shouldBe None
    history.bestHeaderOpt shouldBe Some(block3.header)

    history = applySection(history, block0.adProofs.get)
    history.contains(block0.adProofs.get.id) shouldBe true

    history = applySection(history, block2.adProofs.get)
    history.contains(block2.adProofs.get.id) shouldBe true

    history = applySection(history, block3.adProofs.get)
    history.contains(block3.adProofs.get.id) shouldBe true

    history = applySection(history, block1.adProofs.get)
    history.contains(block1.adProofs.get.id) shouldBe true

    history = applyBlock(history, block0)
    history = applyBlock(history, block1)
    history = applyBlock(history, block2)
    history = applyBlock(history, block3)

    history.bestFullBlockOpt shouldBe Some(block3)
  }

  property("bootstrap from headers and last full blocks") {
    var history = genHistory()._1
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2)

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.value shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None
    history.updateBestFullBlock(chain.last.header)

    val fullBlocksToApply = chain.takeRight(BlocksToKeep)

    history = applyBlock(history, fullBlocksToApply.head)
    history.bestFullBlockOpt.value.header shouldBe fullBlocksToApply.head.header
  }

  property("syncInfo()") {
    val (history, chain) = genHistory(BlocksInChain)

    val si = history.syncInfoV1.asInstanceOf[ErgoSyncInfoV1]
    si.lastHeaderIds.last shouldEqual chain.last.header.id
  }

  property("reportModifierIsValid should set isSemanticallyValid() result") {
    var history = genHistory(1)._1
    history.bestFullBlockOpt.isDefined shouldBe true

    val chain = genChain(BlocksInChain, history).tail
    chain.head.parentId shouldEqual history.bestFullBlockOpt.value.id

    chain.foreach { fullBlock =>
      history.bestHeaderOpt.foreach(b => b.id shouldEqual fullBlock.parentId)
      history.bestFullBlockOpt.foreach(b => b.header shouldBe history.bestHeaderOpt.value)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Absent
      fullBlock.blockSections.foreach(s => history.isSemanticallyValid(s.id) shouldBe Absent)

      history = applyBlock(history, fullBlock)

      history.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
      history.bestHeaderOpt.value.id shouldEqual fullBlock.header.id

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Unknown
      fullBlock.blockSections.foreach(s => history.isSemanticallyValid(s.id) shouldBe Unknown)

      history.reportModifierIsValid(fullBlock.header)
      fullBlock.blockSections.foreach(s => history.reportModifierIsValid(s))

      history.reportModifierIsValid(fullBlock)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Valid
      fullBlock.blockSections.foreach(s => history.isSemanticallyValid(s.id) shouldBe Valid)
    }
  }

  property("reportModifierIsInvalid should set isSemanticallyValid() result for all linked modifiers") {
    var history = genHistory(1)._1

    history.bestFullBlockOpt should not be None

    val chain = genChain(BlocksInChain, history.bestFullBlockOpt.get).tail
    (chain.head.header.parentId == Header.GenesisParentId) shouldBe false

    history = applyChain(history, chain)

    chain.reverse.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Unknown
      history.isSemanticallyValid(fullBlock.adProofs.value.id) shouldBe Unknown
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Unknown


      val progressInfo = ProgressInfo[PM](Option(fullBlock.header.parentId), Seq(fullBlock), Seq.empty, Seq.empty)
      history.reportModifierIsInvalid(fullBlock.header, progressInfo)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.adProofs.value.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }
  }

  property("reportModifierIsInvalid should mark invalid all forks containing this header") {
    var (history, inChain) = genHistory(2)

    val fork1 = genChain(3, history).tail
    val fork2 = genChain(3, history).tail
    fork1.head.parentId shouldEqual fork2.head.parentId

    history = applyChain(history, fork1)
    history = applyChain(history, fork2)

    val progressInfo = ProgressInfo[PM](Some(inChain.last.parentId), fork2, Seq.empty, Seq.empty)
    history.reportModifierIsInvalid(inChain.last.header, progressInfo)

    fork1.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.adProofs.value.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }

    fork2.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.adProofs.value.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }
  }

  property("reportModifierIsInvalid should return blocks to rollback and to process") {
    var history = genHistory(3)._1
    val common = history.bestFullBlockOpt.value

    val fork1 = genChain(3, common).tail
    val fork2Tag = ExtensionCandidate(Seq(Array(0: Byte, 2: Byte) -> Array(1: Byte, 0: Byte)))
    val fork2 = genChain(2, history, extension = defaultExtension ++ fork2Tag).tail

    history = applyChain(history, fork1)
    history = applyChain(history, fork2)

    fork1.length shouldBe 3
    fork2.length shouldBe 2
    fork1.take(2).map(_.header.height) shouldBe fork2.map(_.header.height)
    fork1.head.header.id should not be fork2.head.header.id
    fork1(1).header.id should not be fork2(1).header.id
    fork2.indices.foreach { i =>
      history.bestHeaderIdAtHeight(fork2(i).header.height) shouldBe Some(fork1(i).header.id)
    }
    history.bestHeaderOpt.value shouldBe fork1.last.header

    val progressInfo = ProgressInfo[PM](Some(common.parentId), fork1, Seq.empty, Seq.empty)
    history = history.reportModifierIsInvalid(fork1.head.header, progressInfo).get._1

    history.bestHeaderOpt.value shouldBe fork2.last.header
    fork2.foreach { block =>
      history.bestHeaderIdAtHeight(block.header.height) shouldBe Some(block.header.id)
      history.isInBestChain(block.header) shouldBe true
    }
    history.isInBestChain(fork1.last.header) shouldBe false
    history.bestHeaderIdAtHeight(fork1.last.header.height) shouldBe None
    history.syncInfoV2(full = false).lastHeaders.map(_.id) shouldBe Seq(fork2.last.header.id)
    history.continuationIds(ErgoSyncInfoV2(Seq(common.header)), fork2.length + 1)
      .map(_._2) shouldBe fork2.map(_.header.id)
    history.bestFullBlockOpt.value shouldBe fork2.last
  }

  property("invalidating a header-only tip should select the other header at its height") {
    var history = genHistory(2)._1
    val common = history.bestFullBlockOpt.value.header
    val first = nextHeader(Some(common), history.difficultyCalculator, useRealTs = false)
    val second = nextHeader(Some(common), history.difficultyCalculator,
      tsOpt = Some(first.timestamp + 1), useRealTs = false)
    first.id should not be second.id

    history = history.append(first).get._1
    history = history.append(second).get._1
    history.bestHeaderOpt.value shouldBe first
    val fullBlockBefore = history.bestFullBlockIdOpt

    history = history.reportModifierIsInvalid(first,
      ProgressInfo[PM](Some(common.id), Seq.empty, Seq.empty, Seq.empty)).get._1
    history.bestHeaderOpt.value shouldBe second
    history.bestHeaderIdAtHeight(second.height) shouldBe Some(second.id)
    history.isInBestChain(second) shouldBe true
    history.bestFullBlockIdOpt shouldBe fullBlockBefore
  }

  property("invalidating a header-only best tip marks its semantic validity invalid") {
    var history = genHistory(2)._1
    val common = history.bestFullBlockOpt.value.header
    val tip = nextHeader(Some(common), history.difficultyCalculator, useRealTs = false)

    history = history.append(tip).get._1
    val child = nextHeader(Some(tip), history.difficultyCalculator, useRealTs = false)
    history.bestHeaderOpt.value shouldBe tip
    history.isSemanticallyValid(tip.id) shouldBe Unknown
    history.applicable(child) shouldBe true

    history = history.reportModifierIsInvalid(tip,
      ProgressInfo[PM](Some(common.id), Seq.empty, Seq.empty, Seq.empty)).get._1

    history.bestHeaderOpt.value shouldBe common
    history.isSemanticallyValid(tip.id) shouldBe Invalid
    history.applicable(child) shouldBe false
  }

  property("invalidating the only selected header clears its tip before an equal-score sibling") {
    var history = genHistory()._1
    val first = nextHeader(None, history.difficultyCalculator, useRealTs = false)
    val second = nextHeader(None, history.difficultyCalculator,
      tsOpt = Some(first.timestamp + 1), useRealTs = false)
    first.id should not be second.id

    history = history.append(first).get._1
    history.bestHeaderIdOpt shouldBe Some(first.id)
    history.bestFullBlockIdOpt shouldBe None

    history = history.reportModifierIsInvalid(first,
      ProgressInfo[PM](None, Seq.empty, Seq.empty, Seq.empty)).get._1
    history.bestHeaderIdOpt shouldBe None
    history.isEmpty shouldBe true
    history.isSemanticallyValid(first.id) shouldBe Invalid

    history = history.append(second).get._1
    history.bestHeaderIdOpt shouldBe Some(second.id)
    history.bestHeaderIdAtHeight(second.height) shouldBe Some(second.id)
  }

  property("invalidating the only full genesis clears both selected tips") {
    var history = genHistory()._1
    val first = genChain(1, history).head
    val siblingTag = ExtensionCandidate(Seq(Array(0: Byte, 2: Byte) -> Array(1: Byte, 1: Byte)))
    val second = genChain(1, history, extension = defaultExtension ++ siblingTag).head
    first.id should not be second.id

    history = applyChain(history, Seq(first))
    history.bestHeaderIdOpt shouldBe Some(first.id)
    history.bestFullBlockIdOpt shouldBe Some(first.id)

    val (recovered, progress) = history.reportModifierIsInvalid(first.header,
      ProgressInfo[PM](Some(PreGenesisHeader.id), Seq(first), Seq.empty, Seq.empty)).get
    history = recovered
    progress.branchPoint shouldBe Some(PreGenesisHeader.id)
    progress.toRemove.map(_.id) shouldBe Seq(first.id)
    progress.toApply shouldBe Seq.empty
    history.bestHeaderIdOpt shouldBe None
    history.bestFullBlockIdOpt shouldBe None
    history.isSemanticallyValid(first.id) shouldBe Invalid

    history = applyChain(history, Seq(second))
    history.bestHeaderIdOpt shouldBe Some(second.id)
    history.bestFullBlockIdOpt shouldBe Some(second.id)
  }

  property("invalidating full genesis selects a previously received full sibling") {
    var history = genHistory()._1
    val firstChain = genChain(2, history)
    val first = firstChain.head
    val siblingTag = ExtensionCandidate(Seq(Array(0: Byte, 2: Byte) -> Array(2: Byte, 1: Byte)))
    val second = genChain(1, history, extension = defaultExtension ++ siblingTag).head
    first.id should not be second.id

    history = applyChain(history, firstChain :+ second)
    history.bestHeaderIdOpt shouldBe Some(firstChain.last.id)
    history.bestFullBlockIdOpt shouldBe Some(firstChain.last.id)

    val (recovered, progress) = history.reportModifierIsInvalid(first.header,
      ProgressInfo[PM](Some(PreGenesisHeader.id), firstChain, Seq.empty, Seq.empty)).get
    history = recovered
    progress.branchPoint shouldBe Some(PreGenesisHeader.id)
    progress.toRemove.map(_.id) shouldBe firstChain.map(_.id)
    progress.toApply.map(_.id) shouldBe Seq(second.id)
    history.bestHeaderIdOpt shouldBe Some(second.id)
    history.bestFullBlockIdOpt shouldBe Some(second.id)
    history.bestHeaderIdAtHeight(second.header.height) shouldBe Some(second.id)
    history.isSemanticallyValid(first.id) shouldBe Invalid
  }

  property("invalidating a retained-floor full block clears its selected full tip") {
    var history = genHistory(minFullHeight = Some(3))._1
    val chain = genChain(3, history)
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history = applyBlock(history, chain.last)
    history.bestFullBlockIdOpt shouldBe Some(chain.last.id)

    val (recovered, progress) = history.reportModifierIsInvalid(chain.last.header,
      ProgressInfo[PM](Some(chain(1).id), Seq(chain.last), Seq.empty, Seq.empty)).get
    history = recovered
    progress.toRemove.map(_.id) shouldBe Seq(chain.last.id)
    history.bestFullBlockIdOpt shouldBe None
    history.bestHeaderIdOpt shouldBe Some(chain(1).id)
  }

  for (invalidatedIndex <- Seq(0, 1)) {
    property(s"invalidating retained-floor full block $invalidatedIndex keeps only valid full ancestors") {
      var history = genHistory(minFullHeight = Some(3))._1
      val chain = genChain(4, history)
      history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
      val retained = chain.drop(2)
      history = applyChain(history, retained)
      history.getFullBlock(chain(1).header) shouldBe None
      retained.foreach(block => history.getFullBlock(block.header).map(_.id) shouldBe Some(block.id))
      history.bestFullBlockIdOpt shouldBe Some(retained.last.id)

      val invalidated = retained.drop(invalidatedIndex)
      val (recovered, progress) = history.reportModifierIsInvalid(invalidated.head.header,
        ProgressInfo[PM](Some(invalidated.head.parentId), invalidated, Seq.empty, Seq.empty)).get
      history = recovered

      progress.toRemove.map(_.id) shouldBe invalidated.map(_.id)
      progress.toApply shouldBe Seq.empty
      invalidated.foreach(block => history.isSemanticallyValid(block.id) shouldBe Invalid)
      history.bestHeaderIdOpt shouldBe Some(invalidated.head.parentId)
      if (invalidatedIndex == 0) {
        progress.branchPoint shouldBe Some(PreGenesisHeader.id)
        history.bestFullBlockIdOpt shouldBe None
      } else {
        progress.branchPoint shouldBe Some(retained.head.id)
        history.bestFullBlockIdOpt shouldBe Some(retained.head.id)
        history.isSemanticallyValid(retained.head.id) should not be Invalid
      }
    }
  }

  property("successive invalidations retain a third valid sibling at the same height") {
    var history = genHistory(2)._1
    val common = history.bestFullBlockOpt.value
    val siblings = (0 until 3).map { i =>
      val minerTag = ExtensionCandidate(Seq(Array(0: Byte, 2: Byte) -> Array(i.toByte, 0.toByte)))
      nextBlock(Some(common), common.blockTransactions.txs, defaultExtension ++ minerTag)
    }
    siblings.map(_.header.id).distinct.size shouldBe 3

    history = applyChain(history, siblings.take(2))
    history = history.append(siblings(2).header).get._1
    history.bestHeaderIdOpt shouldBe Some(siblings.head.id)
    history.bestFullBlockIdOpt shouldBe Some(siblings.head.id)

    history = history.reportModifierIsInvalid(siblings.head.header,
      ProgressInfo[PM](Some(common.id), Seq.empty, Seq.empty, Seq.empty)).get._1
    history.isSemanticallyValid(siblings.head.id) shouldBe Invalid
    history.bestHeaderIdOpt shouldBe Some(siblings(1).id)
    history.bestFullBlockIdOpt shouldBe Some(siblings(1).id)

    history = applyChain(history, Seq(siblings(2)))
    history.bestFullBlockIdOpt shouldBe Some(siblings(1).id)

    history = history.reportModifierIsInvalid(siblings(1).header,
      ProgressInfo[PM](Some(common.id), Seq.empty, Seq.empty, Seq.empty)).get._1
    history.bestFullBlockIdOpt shouldBe Some(siblings(2).id)
    history.bestHeaderIdOpt shouldBe Some(siblings(2).id)
  }

  property("reportModifierIsInvalid for non-last block in best chain without better forks") {
    var (history, chain) = genHistory(BlocksInChain)

    history.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
    history.bestHeaderOpt.value shouldEqual chain.last.header

    val invalidChain = chain.takeRight(2)

    val progressInfo = ProgressInfo[PM](Some(invalidChain.head.parentId), invalidChain, Seq.empty, Seq.empty)
    val report = history.reportModifierIsInvalid(invalidChain.head.header, progressInfo).get
    history = report._1
    val processInfo = report._2
    processInfo.toApply.isEmpty shouldBe true
    processInfo.branchPoint.value shouldEqual invalidChain.head.header.parentId
    processInfo.toRemove shouldEqual invalidChain

    history.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
    history.bestHeaderOpt.value.id shouldEqual invalidChain.head.parentId
  }

  property("Report invalid for best full block") {
    val (history, chain) = genHistory(BlocksInChain)

    chain.takeRight(BlocksToKeep - 2).reverse.foreach { fullBlock =>
      history.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
      history.bestHeaderOpt.value shouldEqual fullBlock.header

      val parentHeader = history.typedModifierById[Header](fullBlock.header.parentId).value
      history.contains(parentHeader.transactionsId) shouldBe true
      history.contains(parentHeader.ADProofsId) shouldBe true

      val progressInfo = ProgressInfo[PM](Some(parentHeader.id), Seq(fullBlock), Seq.empty, Seq.empty)
      val (repHistory, _) = history.reportModifierIsInvalid(fullBlock.blockTransactions, progressInfo).get
      repHistory.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
      repHistory.bestHeaderOpt.value shouldBe parentHeader
    }
  }

  property("prune old blocks test") {
    val blocksToPrune = 20

    val (history, chain) = genHistory(BlocksToKeep + blocksToPrune + 1)

    history.bestHeaderOpt.value shouldBe chain.last.header
    history.bestFullBlockOpt.value.header shouldBe chain.last.header

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

  property("prune blocks applied together with the first full block") {
    val votingLength = 4
    var history = genHistory(votingLength = Some(votingLength))._1
    history.bestFullBlockOpt shouldBe None

    val blocksToPrune = 7
    val chain = genChain(BlocksToKeep + blocksToPrune)

    val firstBatch = chain.dropRight(2) // heights 1..16
    val next       = chain.init.last // height 17

    def sectionIds(bs: Seq[ErgoFullBlock]) = bs.flatMap(_.header.sectionIds.map(_._2))
    def stored(bs: Seq[ErgoFullBlock])     = sectionIds(bs).filter(history.contains)
    def missing(bs: Seq[ErgoFullBlock])    = sectionIds(bs).filterNot(history.contains)

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.value shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None

    // Append heights 2..16 before the first full block (height 1) arrives
    firstBatch.tail.foreach { b =>
      history = applyBlock(history, b)
    }
    history.bestFullBlockOpt shouldBe None

    // Applying height 1 queues heights 1..16 for state application without pruning them yet
    history = applyBlock(history, firstBatch.head)
    history.bestFullBlockOpt.value.header shouldBe firstBatch.last.header
    missing(firstBatch) shouldBe empty

    // Next block (height 17): h = 7 > votingLength (4), so the floor rounds down to epoch start 4 and prunes 1..3
    history = applyBlock(history, next)
    history.bestFullBlockOpt.value.header shouldBe next.header
    val (prunedAtNext, keptAtNext) = chain.init.splitAt(votingLength - 1)
    stored(prunedAtNext) shouldBe empty
    missing(keptAtNext) shouldBe empty

    // Next block (height 18): h = 8 reaches the next epoch start, so the floor jumps 4 -> 8 and prunes 4..7
    history = applyBlock(history, chain.last)
    history.bestFullBlockOpt.value.header shouldBe chain.last.header
    val (prunedAtLast, keptAtLast) = chain.splitAt(blocksToPrune)
    stored(prunedAtLast) shouldBe empty
    missing(keptAtLast) shouldBe empty
  }

  property("process fork from genesis") {
    var (history, c) = genHistory(1)
    val genesis = c.head
    val fork1 = genChain(1, history.bestFullBlockOpt.value).tail
    val fork2 = genChain(2, history.bestFullBlockOpt.value).tail

    history = applyChain(history, fork1)
    history.bestHeaderOpt.value shouldBe fork1.last.header

    history = applyChain(history, fork2.dropRight(1))
    val lastBlock = fork2.last
    history = history.append(lastBlock.header).get._1
      .append(lastBlock.blockTransactions).get._1
      .append(lastBlock.extension).get._1

    val changes = history.append(lastBlock.adProofs.value).get
    history = changes._1
    history.bestHeaderOpt.value shouldBe fork2.last.header

    val processInfo = changes._2
    processInfo.branchPoint.get shouldEqual genesis.id
    processInfo.toRemove should contain theSameElementsAs fork1
    processInfo.toApply should contain theSameElementsAs fork2

  }

  property("process fork from existing chain") {
    var history = genHistory(BlocksInChain)._1

    history.bestFullBlockOpt.isDefined should not be None
    forAll(smallPositiveInt) { forkLength: Int =>
      whenever(forkLength > 0) {
        val branchPoint = history.bestFullBlockOpt.value
        val fork1 = genChain(forkLength, branchPoint).tail
        val fork2 = genChain(forkLength + 1, branchPoint).tail

        history = applyChain(history, fork1)
        history.bestHeaderOpt.value shouldBe fork1.last.header

        history = applyChain(history, fork2.dropRight(1))
        val lastBlock = fork2.last
        history = history.append(lastBlock.header).get._1
          .append(lastBlock.extension).get._1
          .append(lastBlock.blockTransactions).get._1

        val changes = history.append(lastBlock.adProofs.value).get
        history = changes._1
        history.bestHeaderOpt.value shouldBe fork2.last.header

        val processInfo = changes._2
        processInfo.branchPoint.value shouldEqual branchPoint.id
        processInfo.toRemove should contain theSameElementsAs fork1
        processInfo.toApply should contain theSameElementsAs fork2
      }
    }
  }

  property("Appended full blocks to best chain in full history") {
    var history = genHistory(1)._1
    history.bestFullBlockOpt.nonEmpty shouldBe true

    val chain = genChain(BlocksInChain, history).tail
    chain.foreach { fullBlock =>
      val startFullBlock = history.bestFullBlockOpt.value
      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      val proofs = fullBlock.adProofs.value
      val extension = fullBlock.extension
      history.contains(header) shouldBe false
      history.contains(txs) shouldBe false
      history.contains(proofs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe true
      history.applicable(proofs) shouldBe false
      history.applicable(txs) shouldBe false
      history.applicable(extension) shouldBe false

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe false
      history.contains(proofs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe true
      history.applicable(txs) shouldBe true
      history.applicable(extension) shouldBe true
      history.bestHeaderOpt.get shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock

      history = history.append(txs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(proofs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe true
      history.applicable(extension) shouldBe true
      history.applicable(txs) shouldBe false
      history.bestHeaderOpt.get shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock

      history = history.append(proofs).get._1
      history = history.append(extension).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(proofs) shouldBe true
      history.contains(extension) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe false
      history.applicable(extension) shouldBe false
      history.applicable(txs) shouldBe false
      history.bestHeaderOpt.value shouldBe header
      history.bestFullBlockOpt.value shouldBe fullBlock
    }
  }

  property("header whose parent is keepVersions or more below the best full block is rejected") {
    // keepVersions controls rollback depth; use a small value (3) to keep the test fast.
    // hdrTooOld fires when fullBlockHeight - parentHeight >= keepVersions.
    val kv = 3
    val inHistory = generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false,
      blocksToKeep = kv + 2, keepVersions = kv)
    inHistory.writeMinimalFullBlockHeight(GenesisHeight)
    inHistory.isHeadersChainSyncedVar = true

    // Apply kv + 1 = 4 full blocks: fullBlockHeight = 4, keepVersions = 3.
    val chain = genChain(kv + 1, inHistory)
    val history = applyChain(inHistory, chain)

    history.fullBlockHeight shouldBe (kv + 1)
    chain.head.header.height shouldBe GenesisHeight

    // Fork from chain.head (height 1). parentHeight = 1.
    // fullBlockHeight - parentHeight = 4 - 1 = 3, which is NOT < keepVersions (3).
    // => hdrTooOld validation rule fires and the header must be rejected.
    val tooDeepHeader = genChain(1, chain.head).tail.head.header // chain.head is head; tail holds the new fork block

    val rejection = history.append(tooDeepHeader)
    rejection.isFailure shouldBe true
    // the validation error carries no rule id, so the rule is identified by its message
    rejection.failed.get.getMessage should include("older than current height minus")

    // Control: fork from the height-2 block. parentHeight = 2.
    // fullBlockHeight - parentHeight = 4 - 2 = 2, which is < keepVersions (3), so the header is accepted.
    val deepEnoughHeader = genChain(1, chain(1)).tail.head.header
    history.append(deepEnoughHeader).isSuccess shouldBe true

    history.bestFullBlockOpt.value shouldBe chain.last
  }

}
