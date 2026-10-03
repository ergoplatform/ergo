package org.ergoplatform.nodeView.history

import org.ergoplatform.consensus.ModifierSemanticValidity
import org.ergoplatform.modifiers.{BlockSection, NonHeaderBlockSection}
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.validation.MalformedModifierError
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId
import scorex.util.encode.Base16

class BlockSectionValidationSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private def changeProofByte(version: Header.Version, outcome: Symbol) = {
    val (history, block) = init(version)
    val bt = block.blockTransactions
    val txBytes = HistoryModifierSerializer.toBytes(bt)

    val txs = bt.transactions
    val proof = txs.head.inputs.head.spendingProof.proof
    proof(0) = if(proof.head < 0) (proof.head + 1).toByte else (proof.head - 1).toByte

    val txBytes2 = HistoryModifierSerializer.toBytes(bt)

    val hashBefore = Base16.encode(Blake2b256(txBytes))
    val hashAfter = Base16.encode(Blake2b256(txBytes2))

    val wrongBt = HistoryModifierSerializer.parseBytes(txBytes2).asInstanceOf[BlockTransactions]

    hashBefore should not be hashAfter
    history.applicableTry(bt) shouldBe 'success
    history.applicableTry(wrongBt) shouldBe outcome
  }

  property("BlockTransactions - proof byte changed - v.1") {
    changeProofByte(Header.InitialVersion, outcome = 'success)
  }

  property("BlockTransactions - proof byte changed - v.2") {
    changeProofByte((Header.InitialVersion + 1).toByte, outcome = 'failure)
  }

  property("BlockTransactions commons check") {
    val (history, block) = init()
    commonChecks(history, block.blockTransactions, block.header)
  }

  property("ADProofs validation") {
    val (history, block) = init()
    commonChecks(history, block.adProofs.get, block.header)
  }

  // History-level guard for bsCorrespondsToHeader. The same scenario through the node view holder
  // is covered by ErgoNodeViewHolderSpec ("Do not apply wrong adProofs").
  property("ADProofs with right headerId but wrong digest are rejected and do not invalidate the header") {
    val (history, block) = init()
    val header = block.header
    val correctProofs = block.adProofs.get

    // Build ADProofs with the right headerId but different proof bytes. Appending a byte
    // (rather than flipping one in place) also works for empty proofs, and changes
    // Algos.hash(proofBytes), so the modifier id differs from header.ADProofsId.
    val wrongBytes: Array[Byte] = correctProofs.proofBytes :+ 0.toByte
    val incorrectProofs = ADProofs(header.id, SerializedAdProof @@ wrongBytes)

    // Sanity: the ID really is different from what the header expects.
    incorrectProofs.id should not equal header.ADProofsId

    // Must be rejected by bsCorrespondsToHeader, which is fatal (MalformedModifierError),
    // not by the recoverable bsNoHeader.
    val rejection = history.applicableTry(incorrectProofs)
    rejection shouldBe 'failure
    rejection.failed.get shouldBe a[MalformedModifierError]

    // Appending the rejected section must not write invalidity for the header or for the
    // sections it commits to, and must not demote the best header (which is what
    // reportModifierIsInvalid does when the header is the best one).
    val bestHeaderBefore = history.bestHeaderIdOpt
    history.append(incorrectProofs) shouldBe 'failure
    Seq(header.id, header.transactionsId, header.ADProofsId).foreach { id =>
      history.isSemanticallyValid(id) should not be ModifierSemanticValidity.Invalid
    }
    history.bestHeaderIdOpt shouldBe bestHeaderBefore

    // Correct proofs must still be accepted after the failed attempt.
    history.applicableTry(correctProofs) shouldBe 'success
  }

  property("Extension validation") {
    val (history, block) = init()
    commonChecks(history, block.extension, block.header)
  }

  private def init(version: Header.Version = Header.InitialVersion) = {
    var history = genHistory()
    val chain = genChain(2, history, version)
    history = applyBlock(history, chain.head)
    history = history.append(chain.last.header).get._1
    (history, chain.last)
  }

  private def commonChecks(history: ErgoHistory, section: NonHeaderBlockSection, header: Header) = {
    history.applicableTry(section) shouldBe 'success
    // header should contain correct digest
    history.applicableTry(withUpdatedHeaderId(section, section.id)) shouldBe 'failure

    // should not be able to apply when blocks at this height are already pruned
    history.applicableTry(section) shouldBe 'success
    history.writeMinimalFullBlockHeight(history.bestHeaderOpt.get.height + 1)
    history.isHeadersChainSyncedVar = true
    history.applicableTry(section) shouldBe 'failure
    history.writeMinimalFullBlockHeight(GenesisHeight)

    // should not be able to apply if corresponding header is marked as invalid
    history.applicableTry(section) shouldBe 'success
    history.historyStorage.insert(Array(history.validityKey(header.id) -> Array(0.toByte)), Array.empty[BlockSection]).get
    history.isSemanticallyValid(header.id) shouldBe ModifierSemanticValidity.Invalid
    history.applicableTry(section) shouldBe 'failure
    history.historyStorage.insert(Array(history.validityKey(header.id) -> Array(1.toByte)), Array.empty[BlockSection]).get

    // should not be able to apply if already in history
    history.applicableTry(section) shouldBe 'success
    history.append(section).get
    history.applicableTry(section) shouldBe 'failure
  }

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  private def withUpdatedHeaderId[T <: NonHeaderBlockSection](section: T, newId: ModifierId): T = section match {
    case s: Extension => s.copy(headerId = newId).asInstanceOf[T]
    case s: BlockTransactions => s.copy(headerId = newId).asInstanceOf[T]
    case s: ADProofs => s.copy(headerId = newId).asInstanceOf[T]
  }

}
