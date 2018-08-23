package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.HistorySpecification
import scorex.core.ModifierId

class BlockSectionValidationSpecification extends HistorySpecification {

  property("BlockTransactions validation") {
    var (history, block) = init()
    commonChecks(history, block.blockTransactions)
  }

  property("ADProofs validation") {
    var (history, block) = init()
    commonChecks(history, block.adProofs.get)
  }

  property("Extension validation") {
    var (history, block) = init()

    val header = block.header
    val extension = block.extension
    val m = extension.mandatoryFields
    val o = extension.optionalFields

    //common checks
    commonChecks(history, extension)

    // validation of mandatory fields key size
    val imvKey = kvGen(Extension.MandatoryFieldKeySize - 1, Extension.MandatoryFieldValueSize).sample.get
    applicableCheck(extension.copy(mandatoryFields = imvKey +: m), header, history)
    // validation of mandatory fields value size
    val imvValue = kvGen(Extension.MandatoryFieldKeySize, Extension.MandatoryFieldValueSize + 1).sample.get
    applicableCheck(extension.copy(mandatoryFields = imvValue +: m), header, history)
    // validation of optional fields key size
    val omvKey = kvGen(Extension.OptionalFieldKeySize - 1, Extension.OptionalFieldValueSize).sample.get
    applicableCheck(extension.copy(optionalFields = omvKey +: o), header, history)
    // validation of optional fields value size
    val omvValue = kvGen(Extension.OptionalFieldKeySize, Extension.OptionalFieldValueSize + 1).sample.get
    applicableCheck(extension.copy(mandatoryFields = omvValue +: o), header, history)
    // validation of optional fields number
    val moreOMV = (0 until Extension.MaxOptionalFields + 1) map (_ => kvGen(Extension.MandatoryFieldKeySize, Extension.MandatoryFieldValueSize).sample.get)
    applicableCheck(extension.copy(mandatoryFields = moreOMV), header, history, correct = true)
    applicableCheck(extension.copy(mandatoryFields = moreOMV ++ o), header, history)
  }


  private def init() = {
    var history = genHistory()
    val chain = genChain(2, history)
    history = applyBlock(history, chain.head)
    history = history.append(chain.last.header).get._1
    (history, chain.last)
  }

  private def commonChecks(history: ErgoHistory, section: BlockSection) = {
    history.applicableTry(section) shouldBe 'success
    history.applicableTry(withUpdatedHeaderId(section, section.id)) shouldBe 'failure
  }

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
