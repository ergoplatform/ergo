package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.Extension
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.HistorySpecification

class BlockSectionValidationSpecification extends HistorySpecification {

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  property("extension validation") {
    var history = genHistory()
    val chain = genChain(2, history)
    history = applyBlock(history, chain.head)

    val block = chain.last
    val extension = block.extension
    val m = extension.mandatoryFields
    val o = extension.optionalFields

    def appendHeader(e: Extension): Extension = {
      val newHeader = block.header.copy(extensionRoot = e.digest)
      history = history.append(newHeader).get._1
      e.copy(headerId = newHeader.id)
    }

    // validation of header id
    history = history.append(block.header).get._1
    history.applicableTry(extension) shouldBe 'success
    history.applicableTry(extension.copy(headerId = extension.id)) shouldBe 'failure

    // validation of mandatory fields key size
    val imvKey = kvGen(Extension.MandatoryFieldKeySize - 1, Extension.MandatoryFieldValueSize).sample.get
    val e2In = extension.copy(mandatoryFields = imvKey +: m)
    val e2 = appendHeader(e2In)
    history.applicableTry(e2) shouldBe 'failure
    // validation of mandatory fields value size
    val imvValue = kvGen(Extension.MandatoryFieldKeySize, Extension.MandatoryFieldValueSize + 1).sample.get
    val e3In = extension.copy(mandatoryFields = imvValue +: m)
    val e3 = appendHeader(e3In)
    history.applicableTry(e3) shouldBe 'failure
    // validation of optional fields key size
    val omvKey = kvGen(Extension.OptionalFieldKeySize - 1, Extension.OptionalFieldValueSize).sample.get
    val e4In = extension.copy(optionalFields = omvKey +: o)
    val e4 = appendHeader(e4In)
    history.applicableTry(e4) shouldBe 'failure
    // validation of optional fields value size
    val omvValue = kvGen(Extension.OptionalFieldKeySize, Extension.OptionalFieldValueSize + 1).sample.get
    val e5In = extension.copy(mandatoryFields = omvValue +: o)
    val e5 = appendHeader(e5In)
    history.applicableTry(e5) shouldBe 'failure
    // validation of optional fields number
    val moreOMV = (0 until Extension.MaxOptionalFields + 1) map (_ => kvGen(Extension.MandatoryFieldKeySize, Extension.MandatoryFieldValueSize).sample.get)
    val e6In = extension.copy(mandatoryFields = moreOMV)
    val e6 = appendHeader(e6In)
    history.applicableTry(e6) shouldBe 'success
    val e7In = extension.copy(mandatoryFields = moreOMV ++ o)
    val e7 = appendHeader(e7In)
    history.applicableTry(e7) shouldBe 'failure
  }


}
