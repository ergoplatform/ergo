package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.state.StateType
import scorex.core.consensus.ModifierSemanticValidity
import scorex.util.ModifierId

class BlockSectionValidationSpecification extends HistoryTestHelpers {

  property("BlockTransactions validation") {
    val (history, block) = init()
    commonChecks(history, block.blockTransactions, block.header)
  }

  property("ADProofs validation") {
    val (history, block) = init()
    commonChecks(history, block.adProofs.get, block.header)
  }

  property("Extension validation") {
    val (history, block) = init()
    commonChecks(history, block.extension, block.header)
  }

  private def init() = {
    var history = genHistory()
    val chain = genChain(2, history)
    history = applyBlock(history, chain.head)
    history = history.append(chain.last.header).get._1
    (history, chain.last)
  }

  private def commonChecks(history: ErgoHistory, section: BlockSection, header: Header) = {
    history.applicableTry(section) shouldBe 'success
    // header should contain correct digest
    history.applicableTry(withUpdatedHeaderId(section, section.id)) shouldBe 'failure

    // should not be able to apply when blocks at this height are already pruned
    history.applicableTry(section) shouldBe 'success
    history.pruningProcessor.minimalFullBlockHeightVar = history.bestHeaderOpt.get.height + 1
    history.pruningProcessor.isHeadersChainSyncedVar = true
    history.applicableTry(section) shouldBe 'failure
    history.pruningProcessor.minimalFullBlockHeightVar = ErgoHistory.GenesisHeight

    // should not be able to apply if corresponding header is marked as invalid
    history.applicableTry(section) shouldBe 'success
    history.storage.update(Seq(history.validityKey(header.id) -> Array(0.toByte)), Seq.empty)
    history.isSemanticallyValid(header.id) shouldBe ModifierSemanticValidity.Invalid
    history.applicableTry(section) shouldBe 'failure
    history.storage.update(Seq(history.validityKey(header.id) -> Array(1.toByte)), Seq.empty)

    // should not be able to apply if already in history
    history.applicableTry(section) shouldBe 'success
    history.append(section).get
    history.applicableTry(section) shouldBe 'failure
  }

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, poPowProve = true, BlocksToKeep)

  private def withUpdatedHeaderId[T <: BlockSection](section: T, newId: ModifierId): T = section match {
    case s: Extension => s.copy(headerId = newId).asInstanceOf[T]
    case s: BlockTransactions => s.copy(headerId = newId).asInstanceOf[T]
    case s: ADProofs => s.copy(headerId = newId).asInstanceOf[T]
  }

}
