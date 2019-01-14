package org.ergoplatform.nodeView.history

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.HistoryTestHelpers
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

    val header = block.header
    val extension = block.extension
    val m = extension.fields

    // checks, specific for extension
    // validation of field keys size
    val imvKey = extensionKvGen(Extension.FieldKeySize - 1, Extension.FieldValueMaxSize).sample.get
    applicableCheck(extension.copy(fields = imvKey +: m), header, history)
    // validation of field value sizes
    val imvValue = extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize + 1).sample.get
    applicableCheck(extension.copy(fields = imvValue +: m), header, history)
    // validation of key duplicates in fields
    val validMKV = extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize).sample.get
    applicableCheck(extension.copy(fields = Seq(validMKV)), header, history, correct = true)
    applicableCheck(extension.copy(fields = Seq(validMKV, validMKV)), header, history)

    // common checks
    commonChecks(history, extension, header)
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
    history.historyStorage.insert(randomKey, Seq(history.validityKey(header.id) -> ByteArrayWrapper(Array(0.toByte))), Seq.empty)
    history.isSemanticallyValid(header.id) shouldBe ModifierSemanticValidity.Invalid
    history.applicableTry(section) shouldBe 'failure
    history.historyStorage.insert(randomKey, Seq(history.validityKey(header.id) -> ByteArrayWrapper(Array(1.toByte))), Seq.empty)

    // should not be able to apply if already in history
    history.applicableTry(section) shouldBe 'success
    history.append(section).get
    history.applicableTry(section) shouldBe 'failure
  }

  private def randomKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash(scorex.utils.Random.randomBytes(32)))

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  private def withUpdatedHeaderId[T <: BlockSection](section: T, newId: ModifierId): T = section match {
    case s: Extension => s.copy(headerId = newId).asInstanceOf[T]
    case s: BlockTransactions => s.copy(headerId = newId).asInstanceOf[T]
    case s: ADProofs => s.copy(headerId = newId).asInstanceOf[T]
  }

  private def withUpdatedSection(section: BlockSection, header: Header): Header = section match {
    case _: Extension => header.copy(extensionRoot = section.digest)
    case _: BlockTransactions => header.copy(transactionsRoot = section.digest)
    case _: ADProofs => header.copy(ADProofsRoot = section.digest)
  }

  /**
    * Incorrect section for `header` should not be applicable
    */
  private def applicableCheck(section: BlockSection,
                              header: Header,
                              history: ErgoHistory,
                              correct: Boolean = false): Unit = {
    val newHeader = withUpdatedSection(section, header)
    val newSection = withUpdatedHeaderId(section, newHeader.id)
    history.append(newHeader).get._1
    if (correct) {
      history.applicableTry(newSection) shouldBe 'success
    } else {
      history.applicableTry(newSection) shouldBe 'failure
    }
  }

}
