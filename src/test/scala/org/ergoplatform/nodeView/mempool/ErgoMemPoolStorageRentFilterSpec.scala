package org.ergoplatform.nodeView.mempool

import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.BoxHolder
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.{ErgoStateContextHelpers, ErgoTestHelpers}
import org.ergoplatform.wallet.utils.TestFileUtils
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.ast.ShortConstant
import sigma.interpreter.{ContextExtension, ProverResult}

/**
  * Pins the invariants of the mempool policy filter rejecting storage rent collection
  * transactions (`ErgoMemPool.containsStorageRentClaim`).
  *
  * A storage rent claim spends a box via an empty spending proof and the storage-rent-specific
  * context extension variable #127 (index of the recreated output). Storage rent is to be
  * collected by miners directly during candidate block generation, not relayed through the
  * mempool. The filter is a mempool policy only: consensus rules are unchanged and the same
  * transactions remain valid in blocks.
  */
class ErgoMemPoolStorageRentFilterSpec extends AnyFlatSpec
  with Matchers
  with ErgoTestHelpers
  with ErgoStateContextHelpers
  with TestFileUtils {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val BoxValue: Long = 10L * CoinsInOneErgo

  private def box(value: Long, tree: ErgoTree, index: Short): ErgoBox =
    new ErgoBox(
      value,
      tree,
      Colls.emptyColl,
      Map.empty,
      bytesToId(Array.fill(32)(1.toByte)),
      index,
      creationHeight = 0
    )

  private def candidate(value: Long): ErgoBoxCandidate =
    new ErgoBoxCandidate(value, Constants.TrueTree, 0, Colls.emptyColl, Map.empty)

  private def rentInput(b: ErgoBox, outIndex: Short = 0): Input =
    Input(b.id, ProverResult(
      Array.emptyByteArray,
      ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(outIndex)))
    ))

  private def rentClaim(b: ErgoBox): ErgoTransaction =
    ErgoTransaction(IndexedSeq(rentInput(b)), IndexedSeq.empty, IndexedSeq(candidate(b.value)))

  private case class Fixture(s: ErgoSettings,
                             state: WrappedUtxoState,
                             pool: ErgoMemPool,
                             rentBox: ErgoBox,
                             plainBox: ErgoBox)

  private def fixture(): Fixture = {
    val s = settings
    val rentBox = box(BoxValue, Constants.TrueTree, 0)
    val plainBox = box(BoxValue, Constants.TrueTree, 1)
    val state = WrappedUtxoState(BoxHolder(Seq(rentBox, plainBox)), createTempDir,
      s.launchParameters, s)
    Fixture(s, state, ErgoMemPool.empty(s), rentBox, plainBox)
  }

  private def process(f: Fixture, tx: ErgoTransaction): (ErgoMemPool, ProcessingOutcome) =
    f.pool.process(UnconfirmedTransaction(tx, None), f.state)

  private val FilterReason = "Mempool policy declines a storage rent collection transaction"

  private def declinedByFilter(outcome: ProcessingOutcome): Boolean = outcome match {
    case d: ProcessingOutcome.Declined => Option(d.e.getMessage).exists(_.contains(FilterReason))
    case _ => false
  }

  // ------------------------------------------------------------------ (a)

  it should "(a) decline a transaction using the storage-rent extension variable" in {
    val f = fixture()
    val tx = rentClaim(f.rentBox)

    val (updPool, outcome) = process(f, tx)

    outcome shouldBe a[ProcessingOutcome.Declined]
    outcome shouldNot be(a[ProcessingOutcome.Invalidated])
    declinedByFilter(outcome) shouldBe true
    updPool.size shouldBe 0
    updPool.isInvalidated(tx.id) shouldBe true

    // The input box is not blacklisted: an ordinary spend of the same box is still admissible.
    val ordinary = ErgoTransaction(
      IndexedSeq(Input(f.rentBox.id, ProverResult.empty)),
      IndexedSeq.empty,
      IndexedSeq(candidate(f.rentBox.value))
    )
    val (_, reuse) = updPool.process(UnconfirmedTransaction(ordinary, None), f.state)
    reuse shouldBe a[ProcessingOutcome.Accepted]
  }

  // ------------------------------------------------------------------ (b)

  it should "(b) decline pre-validation, without reaching the interpreter" in {
    val f = fixture()
    // The box is guarded by an unspendable script: had the filter abstained, input script
    // verification would fail and the outcome would be an Invalidated instead.
    val guardedBox = box(BoxValue, Constants.FalseTree, 2)
    val state = WrappedUtxoState(BoxHolder(Seq(guardedBox)), createTempDir, f.s.launchParameters, f.s)

    val (_, outcome) = f.pool.process(UnconfirmedTransaction(rentClaim(guardedBox), None), state)
    declinedByFilter(outcome) shouldBe true
    outcome shouldNot be(a[ProcessingOutcome.Invalidated])
  }

  // ------------------------------------------------------------------ (c)

  it should "(c) pin the predicate firing boundaries" in {
    val pool = ErgoMemPool.empty(settings)
    val b = box(BoxValue, Constants.TrueTree, 0)

    // Fires on the rent-claim shape: empty proof + variable #127.
    pool.containsStorageRentClaim(rentClaim(b)) shouldBe true

    // Abstains on a non-empty spending proof (an owner spend carrying the variable).
    val withProof = ErgoTransaction(
      IndexedSeq(Input(b.id, ProverResult(
        Array.fill(2)(1.toByte),
        ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))
      ))),
      IndexedSeq.empty,
      IndexedSeq(candidate(BoxValue))
    )
    pool.containsStorageRentClaim(withProof) shouldBe false

    // Abstains on an empty proof without the #127 variable.
    val noVar = ErgoTransaction(
      IndexedSeq(Input(b.id, ProverResult.empty)),
      IndexedSeq.empty,
      IndexedSeq(candidate(BoxValue))
    )
    pool.containsStorageRentClaim(noVar) shouldBe false

    // Fires on a mixed transaction: a rent claim can not be laundered alongside ordinary inputs.
    val plainBox = box(BoxValue, Constants.TrueTree, 1)
    val mixed = ErgoTransaction(
      IndexedSeq(Input(plainBox.id, ProverResult.empty), rentInput(b)),
      IndexedSeq.empty,
      IndexedSeq(candidate(2 * BoxValue))
    )
    pool.containsStorageRentClaim(mixed) shouldBe true
  }

  // ------------------------------------------------------------------ (d)

  it should "(d) accept an ordinary transaction" in {
    val f = fixture()
    val ordinary = ErgoTransaction(
      IndexedSeq(Input(f.plainBox.id, ProverResult.empty)),
      IndexedSeq.empty,
      IndexedSeq(candidate(f.plainBox.value))
    )

    val (updPool, outcome) = process(f, ordinary)
    outcome shouldBe a[ProcessingOutcome.Accepted]
    updPool.isInvalidated(ordinary.id) shouldBe false
  }

}
