package org.ergoplatform.nodeView.history

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.HistorySpecification
import scorex.core.ModifierId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity

class BlockSectionValidationSpecification extends HistorySpecification {

  property("BlockTransactions validation") {
    var (history, block) = init()
    commonChecks(history, block.blockTransactions, block.header)
  }

  property("ADProofs validation") {
    var (history, block) = init()
    commonChecks(history, block.adProofs.get, block.header)
  }

  property("Extension validation") {
    var (history, block) = init()

    val header = block.header
    val extension = block.extension
    val m = extension.mandatoryFields
    val o = extension.optionalFields

    // checks, specific for extension
    // validation of mandatory fields key size
    val imvKey = kvGen(Extension.MandatoryFieldKeySize - 1, Extension.MaxMandatoryFieldValueSize).sample.get
    applicableCheck(extension.copy(mandatoryFields = imvKey +: m), header, history)
    // validation of mandatory fields value size
    val imvValue = kvGen(Extension.MandatoryFieldKeySize, Extension.MaxMandatoryFieldValueSize + 1).sample.get
    applicableCheck(extension.copy(mandatoryFields = imvValue +: m), header, history)
    // validation of optional fields key size
    val omvKey = kvGen(Extension.OptionalFieldKeySize - 1, Extension.MaxOptionalFieldValueSize).sample.get
    applicableCheck(extension.copy(optionalFields = omvKey +: o), header, history)
    // validation of optional fields value size
    val omvValue = kvGen(Extension.OptionalFieldKeySize, Extension.MaxOptionalFieldValueSize + 1).sample.get
    applicableCheck(extension.copy(mandatoryFields = omvValue +: o), header, history)
    // validation of optional fields number
    val moreOMV = (0 until Extension.MaxOptionalFields + 1) map (_ => kvGen(Extension.MandatoryFieldKeySize, Extension.MaxMandatoryFieldValueSize).sample.get)
    applicableCheck(extension.copy(mandatoryFields = moreOMV), header, history, correct = true)
    applicableCheck(extension.copy(mandatoryFields = moreOMV ++ o), header, history)

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
    history.applicableTry(section) shouldBe 'failure
    history.pruningProcessor.minimalFullBlockHeightVar = 0

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
