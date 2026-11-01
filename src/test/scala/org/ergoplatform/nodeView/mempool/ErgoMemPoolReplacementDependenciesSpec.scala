package org.ergoplatform.nodeView.mempool

import org.ergoplatform.{DataInput, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.{ProcessingOutcome, SortingOption}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.flatspec.AnyFlatSpec

class ErgoMemPoolReplacementDependenciesSpec extends AnyFlatSpec with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  it should "retain providers needed by a replacement and accept an independent control" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val byteSortSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(mempoolSorting = SortingOption.FeePerByte)
    )
    val feeProp = settings.chainSettings.monetary.feeProposition
    val parentFee = math.max(settings.nodeSettings.minimalFeeAmount, 1000000L)
    val replacementFee = parentFee * 8L
    val inputBox = wus.takeBoxes(100)
      .find(b => b.ergoTree == TrueTree && b.value > replacementFee).get
    val input = new Input(inputBox.id, emptyProverResult)
    val validationContext = wus.stateContext.simplifiedUpcoming()

    // P spends a confirmed X and creates unconfirmed Y.
    val parent = ErgoTransaction(
      IndexedSeq(input),
      IndexedSeq(
        new ErgoBoxCandidate(inputBox.value - parentFee, TrueTree, creationHeight = 0),
        new ErgoBoxCandidate(parentFee, feeProp, creationHeight = 0)
      )
    )
    val y = parent.outputs.head
    val replacementOutputs = IndexedSeq(
      new ErgoBoxCandidate(inputBox.value - replacementFee, TrueTree, creationHeight = 0),
      new ErgoBoxCandidate(replacementFee, feeProp, creationHeight = 0)
    )
    val (withParent, parentOutcome) = ErgoMemPool.empty(byteSortSettings)
      .process(UnconfirmedTransaction(parent, None), wus)
    parentOutcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    wus.boxById(y.id) shouldBe None

    def expectDeclined(pool: ErgoMemPool, replacement: ErgoTransaction): Unit = {
      wus.withMempool(pool).validateWithCost(
        replacement, validationContext, Int.MaxValue, None).isSuccess shouldBe true
      val (after, outcome) = pool.process(UnconfirmedTransaction(replacement, None), wus)
      outcome.isInstanceOf[ProcessingOutcome.Declined] shouldBe true
      outcome.asInstanceOf[ProcessingOutcome.Declined].e.getMessage shouldBe
        ErgoMemPool.availabilityDeclineReason(ErgoMemPool.Missing(y.id)).get
      after.contains(parent.id) shouldBe true
      after.contains(replacement.id) shouldBe false
    }

    // R initially resolves Y, but RBF would evict its producer P.
    val direct = ErgoTransaction(
      IndexedSeq(input), IndexedSeq(DataInput(y.id)), replacementOutputs)
    expectDeclined(withParent, direct)

    // A full pool can evict P even when the new transaction spends another box.
    val secondBox = wus.takeBoxes(100).find { b =>
      b.ergoTree == TrueTree && b.value > replacementFee && !b.id.sameElements(inputBox.id)
    }.get
    val capacitySettings = byteSortSettings.copy(
      nodeSettings = byteSortSettings.nodeSettings.copy(mempoolCapacity = 1)
    )
    val (capacityWithParent, capacityParentOutcome) = ErgoMemPool.empty(capacitySettings)
      .process(UnconfirmedTransaction(parent, None), wus)
    capacityParentOutcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    val capacityReplacement = ErgoTransaction(
      IndexedSeq(new Input(secondBox.id, emptyProverResult)),
      IndexedSeq(DataInput(y.id)),
      IndexedSeq(
        new ErgoBoxCandidate(secondBox.value - replacementFee, TrueTree, creationHeight = 0),
        new ErgoBoxCandidate(replacementFee, feeProp, creationHeight = 0)
      )
    )
    expectDeclined(capacityWithParent, capacityReplacement)

    // P merely spends this confirmed data box; its eviction does not erase that box.
    val confirmedDataReplacement = ErgoTransaction(
      capacityReplacement.inputs,
      IndexedSeq(DataInput(inputBox.id)),
      capacityReplacement.outputCandidates
    )
    val (afterConfirmed, confirmedOutcome) = capacityWithParent.process(
      UnconfirmedTransaction(confirmedDataReplacement, None), wus)
    confirmedOutcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    afterConfirmed.contains(parent.id) shouldBe false
    wus.withMempool(afterConfirmed).validateWithCost(
      confirmedDataReplacement, validationContext, Int.MaxValue, None).isSuccess shouldBe true

    // Q spends Y and creates Z. The provisional pool still contains Q after removing P,
    // so checking only whether Z is present misses its lost ancestor Y.
    val child = ErgoTransaction(
      IndexedSeq(new Input(y.id, emptyProverResult)),
      IndexedSeq(
        new ErgoBoxCandidate(y.value - parentFee, TrueTree, creationHeight = 0),
        new ErgoBoxCandidate(parentFee, feeProp, creationHeight = 0)
      )
    )
    val (withChild, childOutcome) = withParent.process(UnconfirmedTransaction(child, None), wus)
    childOutcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    val z = child.outputs.head
    val transitiveData = ErgoTransaction(
      IndexedSeq(input), IndexedSeq(DataInput(z.id)), replacementOutputs)
    expectDeclined(withChild, transitiveData)

    // The same missing ancestor matters when R spends Z as a regular input.
    val transitiveSpend = ErgoTransaction(
      IndexedSeq(input, new Input(z.id, emptyProverResult)),
      IndexedSeq(
        new ErgoBoxCandidate(inputBox.value - replacementFee, TrueTree, creationHeight = 0),
        new ErgoBoxCandidate(z.value, TrueTree, creationHeight = 0),
        new ErgoBoxCandidate(replacementFee, feeProp, creationHeight = 0)
      )
    )
    expectDeclined(withChild, transitiveSpend)

    val noDataInputControl = ErgoTransaction(IndexedSeq(input), replacementOutputs)
    val (afterControl, controlOutcome) = withParent.process(
      UnconfirmedTransaction(noDataInputControl, None), wus)
    controlOutcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    afterControl.contains(parent.id) shouldBe false
    afterControl.contains(noDataInputControl.id) shouldBe true
    wus.withMempool(afterControl).validateWithCost(
      noDataInputControl, validationContext, Int.MaxValue, None).isSuccess shouldBe true
  }
}
