package org.ergoplatform.nodeView.mempool

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.Timeout
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedMempool
import org.ergoplatform.nodeView.UtxoNodeViewHolder
import org.ergoplatform.nodeView.ErgoNodeViewHolder.CurrentView
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.{ErgoStateContextHelpers, ErgoTestHelpers}
import org.ergoplatform.wallet.utils.TestFileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.Digest32Coll
import sigma.interpreter.ProverResult
import sigmastate.utils.Extensions.ModifierIdOps

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Pins the invariants of the mempool policy prefilter for the invalid token-preserving
  * re-emission shape (`ErgoMemPool.preservesReemissionTokens`).
  *
  * These are also the first tests in the repository to exercise the `verifyReemissionSpending`
  * shape at all - see the suite-level comment on invariant (d1) for why the emission transaction
  * is covered at predicate level rather than end to end.
  */
class ErgoMemPoolReemissionPrefilterSpec extends AnyFlatSpec
  with Matchers
  with ErgoTestHelpers
  with ErgoStateContextHelpers
  with TestFileUtils {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  // Synthetic identifiers - deliberately not any real chain's token ids.
  private val ReemissionTokenBytes: Array[Byte] = Array.fill(32)(7.toByte)
  private val ReemissionToken: ModifierId = bytesToId(ReemissionTokenBytes)
  private val EmissionNft: ModifierId = bytesToId(Array.fill(32)(9.toByte))

  /**
    * The end-to-end fixture state is built straight from a `BoxHolder`, so it sits at
    * `EmptyHistoryHeight`. Conjunct (1) of the predicate is a strict `>`, so activation must be
    * strictly below that for the filter to be reachable at all.
    */
  private val FixtureHeight: Int = ErgoHistoryUtils.EmptyHistoryHeight
  private val FixtureActivationHeight: Int = FixtureHeight - 1

  // Heights used by the predicate-level tests, where the context height is chosen directly.
  private val UnitHeight: Int = 1000
  private val UnitActivationHeight: Int = 100

  private def withReemission(tokenId: ModifierId,
                             checkRules: Boolean,
                             activationHeight: Int): ErgoSettings = {
    val rs = settings.chainSettings.reemission.copy(
      checkReemissionRules = checkRules,
      emissionNftId = EmissionNft,
      reemissionTokenId = tokenId,
      activationHeight = activationHeight
    )
    settings.copy(chainSettings = settings.chainSettings.copy(reemission = rs))
  }

  private def tokenOf(s: ErgoSettings, amount: Long = 5L): (Digest32Coll, Long) =
    (Digest32Coll @@ s.chainSettings.reemission.reemissionTokenId.toColl) -> amount

  private def emissionNftOf(s: ErgoSettings): (Digest32Coll, Long) =
    (Digest32Coll @@ s.chainSettings.reemission.emissionNftId.toColl) -> 1L

  private def box(value: Long,
                  tree: ErgoTree,
                  tokens: Seq[(Digest32Coll, Long)],
                  index: Short): ErgoBox =
    new ErgoBox(
      value,
      tree,
      Colls.fromArray(tokens.toArray),
      Map.empty,
      bytesToId(Array.fill(32)(1.toByte)),
      index,
      creationHeight = 0
    )

  private def candidate(value: Long, tree: ErgoTree, tokens: Seq[(Digest32Coll, Long)]): ErgoBoxCandidate =
    new ErgoBoxCandidate(value, tree, creationHeight = 0, Colls.fromArray(tokens.toArray), Map.empty)

  private def txSpending(inputs: Seq[ErgoBox], outputs: Seq[ErgoBoxCandidate]): ErgoTransaction =
    ErgoTransaction(
      inputs.map(b => new Input(b.id, ProverResult.empty)).toIndexedSeq,
      outputs.toIndexedSeq
    )

  private val BoxValue: Long = 10L * CoinsInOneErgo

  /** Fixture: a UTXO state holding a token-bearing box, a token-bearing box behind a false
    * script, and a plain box - all reachable by `process`. */
  private case class Fixture(s: ErgoSettings,
                             state: WrappedUtxoState,
                             pool: ErgoMemPool,
                             tokenBox: ErgoBox,
                             guardedTokenBox: ErgoBox,
                             plainBox: ErgoBox)

  private def fixture(checkRules: Boolean): Fixture = {
    val s = withReemission(ReemissionToken, checkRules, FixtureActivationHeight)
    val tokenBox = box(BoxValue, Constants.TrueTree, Seq(tokenOf(s)), 0)
    // Spending this one would fail script verification loudly if the interpreter ever ran.
    val guardedTokenBox = box(BoxValue, Constants.FalseTree, Seq(tokenOf(s)), 1)
    val plainBox = box(BoxValue, Constants.TrueTree, Seq.empty, 2)
    val bh = BoxHolder(Seq(tokenBox, guardedTokenBox, plainBox))
    val state = WrappedUtxoState(bh, createTempDir, settings.launchParameters, s)
    Fixture(s, state, ErgoMemPool.empty(s), tokenBox, guardedTokenBox, plainBox)
  }

  private def preserving(f: Fixture, in: ErgoBox): ErgoTransaction =
    txSpending(Seq(in), Seq(candidate(in.value, Constants.TrueTree, Seq(tokenOf(f.s)))))

  private def dropping(f: Fixture, in: ErgoBox): ErgoTransaction =
    txSpending(Seq(in), Seq(candidate(in.value, Constants.TrueTree, Seq.empty)))

  private def process(f: Fixture, tx: ErgoTransaction): (ErgoMemPool, ProcessingOutcome) =
    f.pool.process(UnconfirmedTransaction(tx, None), f.state)

  private val PrefilterReason =
    "Mempool policy declines a token-preserving re-emission spend on the non-emission path"

  private def declinedByPrefilter(outcome: ProcessingOutcome): Boolean = outcome match {
    case d: ProcessingOutcome.Declined => Option(d.e.getMessage).exists(_.contains(PrefilterReason))
    case _ => false
  }

  // ------------------------------------------------------------------ (a)

  it should "(a) decline the token-preserving non-emission shape under both checkReemissionRules settings" in {
    Seq(true, false).foreach { checkRules =>
      val f = fixture(checkRules)
      val (updPool, outcome) = process(f, preserving(f, f.tokenBox))

      // Asserting the *reason*, not merely non-Accepted: with checkReemissionRules = true an
      // EIP-27 failure would also reject this transaction, but as an Invalidated - so a bare
      // non-Accepted check would pass for the wrong reason on one of the two settings.
      withClue(s"checkReemissionRules = $checkRules: ") {
        outcome shouldBe a[ProcessingOutcome.Declined]
        declinedByPrefilter(outcome) shouldBe true
        updPool.size shouldBe 0
      }
    }
  }

  // ------------------------------------------------------------------ (b)

  it should "(b) reject before script execution and cache the rejection by transaction id" in {
    val f = fixture(checkRules = false)
    // guardedTokenBox is locked by Constants.FalseTree: if the prefilter did not short-circuit,
    // input script verification would fail and the outcome would be an Invalidated instead.
    val tx = preserving(f, f.guardedTokenBox)
    val (updPool, outcome) = process(f, tx)

    declinedByPrefilter(outcome) shouldBe true
    outcome shouldNot be(a[ProcessingOutcome.Invalidated])

    updPool.isInvalidated(tx.id) shouldBe true
  }

  // ------------------------------------------------------------------ (c)

  it should "(c) produce Declined rather than Invalidated, so no MisbehaviorPenalty is applied" in {
    val f = fixture(checkRules = false)
    val (_, outcome) = process(f, preserving(f, f.tokenBox))

    // Structural guarantee behind this outcome-level assertion (SPEC v4 section 2.4):
    //   ErgoNodeViewHolder.scala:279-282   Declined            -> DeclinedTransaction
    //   ErgoNodeViewSynchronizer.scala:1461-1479
    //       DeclinedTransaction is handled with no PenalizePeer message; only FailedTransaction
    //       (i.e. ProcessingOutcome.Invalidated) reaches penalizeMisbehavingPeer.
    // So pinning the outcome type pins the absence of the penalty.
    outcome shouldBe a[ProcessingOutcome.Declined]
    outcome shouldNot be(a[ProcessingOutcome.Invalidated])
  }

  // ------------------------------------------------------------------ (d1)

  it should "(d1) abstain at predicate level on every shape the filter must not touch" in {
    val s = withReemission(ReemissionToken, checkRules = true, UnitActivationHeight)
    val pool = ErgoMemPool.empty(s)
    val ctx = stateContext(UnitHeight, blockVersion = 1, s)
    val token = tokenOf(s)

    // Emission transaction: input far above the 100K ERG bar, outputs legitimately carrying
    // both the emission NFT and the re-emission token. Kept at predicate level deliberately -
    // synthesising a *valid* emission transaction end to end would test the fixture, not the filter.
    // Note the abstention here comes from the >100K conjunct alone: the predicate has no emission
    // NFT logic at all, so the NFT on this fixture is decorative and only makes the shape realistic.
    val emissionIn = box(200000L * CoinsInOneErgo, Constants.TrueTree, Seq(emissionNftOf(s), token), 0)
    val emissionOut = candidate(200000L * CoinsInOneErgo, Constants.TrueTree, Seq(emissionNftOf(s), token))
    pool.preservesReemissionTokens(Seq(emissionIn), Seq(emissionOut), ctx) shouldBe false

    // Exactly 100K ERG: the source bar is a strict `>`, so this is a non-emission input and the
    // filter is expected to fire - asserted in (g). Here we only pin the mixed case below.

    // Mixed transaction: one input above the bar alongside a small token-bearing one.
    val smallIn = box(BoxValue, Constants.TrueTree, Seq(token), 1)
    val outWithToken = candidate(BoxValue, Constants.TrueTree, Seq(token))
    pool.preservesReemissionTokens(Seq(emissionIn, smallIn), Seq(outWithToken), ctx) shouldBe false

    // Activation-height boundary: conjunct (1) is a strict `>`.
    val atActivation = stateContext(UnitActivationHeight, blockVersion = 1, s)
    pool.preservesReemissionTokens(Seq(smallIn), Seq(outWithToken), atActivation) shouldBe false

    // Chain without EIP-27 configured: empty token id makes the predicate structurally inert.
    val noEip27 = withReemission(bytesToId(Array.emptyByteArray), checkRules = true, UnitActivationHeight)
    val inertPool = ErgoMemPool.empty(noEip27)
    val inertCtx = stateContext(UnitHeight, blockVersion = 1, noEip27)
    inertPool.preservesReemissionTokens(Seq(smallIn), Seq(outWithToken), inertCtx) shouldBe false

    // Ordinary transaction carrying no re-emission token at all.
    val plainIn = box(BoxValue, Constants.TrueTree, Seq.empty, 2)
    val plainOut = candidate(BoxValue, Constants.TrueTree, Seq.empty)
    pool.preservesReemissionTokens(Seq(plainIn), Seq(plainOut), ctx) shouldBe false
  }

  // ------------------------------------------------------------------ (d2)

  it should "(d2) still accept a conformant owner spend and an ordinary transaction end to end" in {
    val f = fixture(checkRules = false)

    val (poolAfterDrop, dropOutcome) = process(f, dropping(f, f.tokenBox))
    dropOutcome shouldBe a[ProcessingOutcome.Accepted]
    poolAfterDrop.isInvalidated(dropping(f, f.tokenBox).id) shouldBe false

    val ordinary = txSpending(Seq(f.plainBox), Seq(candidate(f.plainBox.value, Constants.TrueTree, Seq.empty)))
    val (poolAfterPlain, plainOutcome) = process(f, ordinary)
    plainOutcome shouldBe a[ProcessingOutcome.Accepted]
    poolAfterPlain.isInvalidated(ordinary.id) shouldBe false
  }

  // ------------------------------------------------------------------ (d3)

  it should "(d3) accept a fully conformant owner spend end to end with checkReemissionRules = true" in {
    // The burn obligation is denominated 1 nanoErg per re-emission token, so the token amount is
    // also the ERG amount owed to the pay-to-re-emission contract.
    val BurnAmount = 1000000L

    val s = withReemission(ReemissionToken, checkRules = true, FixtureActivationHeight)
    val tokenBox = box(BoxValue, Constants.TrueTree, Seq(tokenOf(s, BurnAmount)), 0)
    val state = WrappedUtxoState(BoxHolder(Seq(tokenBox)), createTempDir, settings.launchParameters, s)
    val pool = ErgoMemPool.empty(s)

    // Token dropped from the recreated output, and exactly the owed amount paid to the configured
    // pay-to-re-emission proposition - so both obligations of the non-emission branch are met.
    val payToReemission = s.chainSettings.reemission.reemissionRules.payToReemission
    val tx = txSpending(
      Seq(tokenBox),
      Seq(
        candidate(BoxValue - BurnAmount, Constants.TrueTree, Seq.empty),
        candidate(BurnAmount, payToReemission, Seq.empty)
      )
    )

    // This is the case that actually reaches verifyReemissionSpending and passes it: with
    // checkReemissionRules = true the txReemission rule is evaluated rather than short-circuited,
    // so an Accepted outcome here means the EIP-27 non-emission branch was satisfied, not skipped.
    val (updPool, outcome) = pool.process(UnconfirmedTransaction(tx, None), state)
    outcome shouldBe a[ProcessingOutcome.Accepted]
    updPool.isInvalidated(tx.id) shouldBe false
  }

  // ------------------------------------------------------------------ (e)

  it should "(e) not blacklist the input box of a rejected transaction" in {
    val f = fixture(checkRules = false)

    // Seed the pool so the inputs/outputs maps are non-empty and a regression would be visible.
    val seeded = process(f, txSpending(Seq(f.plainBox), Seq(candidate(f.plainBox.value, Constants.TrueTree, Seq.empty))))._1
    val inputsBefore = seeded.pool.inputs
    val outputsBefore = seeded.pool.outputs

    val (afterReject, outcome) = seeded.process(UnconfirmedTransaction(preserving(f, f.tokenBox), None), f.state)
    declinedByPrefilter(outcome) shouldBe true

    afterReject.pool.inputs shouldBe inputsBefore
    afterReject.pool.outputs shouldBe outputsBefore

    // The same input box remains spendable by a well-formed transaction.
    val (_, reuse) = afterReject.process(UnconfirmedTransaction(dropping(f, f.tokenBox), None), f.state)
    reuse shouldBe a[ProcessingOutcome.Accepted]
  }

  // ------------------------------------------------------------------ (f)

  it should "(f) leave the post-2438 token-dropping claim shape admissible" in {
    val s = withReemission(ReemissionToken, checkRules = true, UnitActivationHeight)
    val pool = ErgoMemPool.empty(s)
    val ctx = stateContext(UnitHeight, blockVersion = 1, s)

    // Token-bearing input under the bar, token dropped from the recreated output: conjunct (4)
    // is false, so the filter abstains and this stays composable with #2438.
    val in = box(BoxValue, Constants.TrueTree, Seq(tokenOf(s)), 0)
    val recreated = candidate(BoxValue - CoinsInOneErgo, Constants.TrueTree, Seq.empty)
    val burnPayment = candidate(CoinsInOneErgo, Constants.TrueTree, Seq.empty)

    pool.preservesReemissionTokens(Seq(in), Seq(recreated, burnPayment), ctx) shouldBe false
  }

  // ------------------------------------------------------------------ (g)

  it should "(g) pin the four boundary regressions" in {
    val s = withReemission(ReemissionToken, checkRules = true, UnitActivationHeight)
    val pool = ErgoMemPool.empty(s)
    val ctx = stateContext(UnitHeight, blockVersion = 1, s)
    val token = tokenOf(s)
    val outWithToken = candidate(BoxValue, Constants.TrueTree, Seq(token))

    // (i) Exactly 100K ERG: ErgoTransaction.scala:255 tests a strict `>`, so such a box falls to
    // the non-emission branch at :305 and sets reemissionSpending. The filter must FIRE here.
    // Pinning the direction is the point: flipping either comparison to `>=` during a later
    // tidy-up would desynchronise the filter from the source, and this assertion catches it.
    val exactly100K = box(100000L * CoinsInOneErgo, Constants.TrueTree, Seq(token), 0)
    pool.preservesReemissionTokens(Seq(exactly100K), Seq(outWithToken), ctx) shouldBe true

    // (ii) Mixed transaction abstains, per conjunct (2)'s !exists formulation.
    val aboveBar = box(100000L * CoinsInOneErgo + 1, Constants.TrueTree, Seq(token), 1)
    val underBar = box(BoxValue, Constants.TrueTree, Seq(token), 2)
    pool.preservesReemissionTokens(Seq(aboveBar, underBar), Seq(outWithToken), ctx) shouldBe false

    // (iii) currentHeight == activationHeight abstains, per conjunct (1)'s strict `>`.
    val atActivation = stateContext(UnitActivationHeight, blockVersion = 1, s)
    pool.preservesReemissionTokens(Seq(underBar), Seq(outWithToken), atActivation) shouldBe false
    // ... and fires one block later, confirming the boundary is where it is claimed to be.
    val afterActivation = stateContext(UnitActivationHeight + 1, blockVersion = 1, s)
    pool.preservesReemissionTokens(Seq(underBar), Seq(outWithToken), afterActivation) shouldBe true

    // (iv) Empty reemissionTokenId leaves the predicate inert, per conjunct (0).
    val noEip27 = withReemission(bytesToId(Array.emptyByteArray), checkRules = true, UnitActivationHeight)
    val inertPool = ErgoMemPool.empty(noEip27)
    val inertCtx = stateContext(UnitHeight, blockVersion = 1, noEip27)
    inertPool.preservesReemissionTokens(Seq(underBar), Seq(outWithToken), inertCtx) shouldBe false
  }

  // ------------------------------------------------------------------ (h)

  /**
    * Drives the rejection through the real ErgoNodeViewHolder actor to assert on the *installed*
    * pool, not just on `process`'s return value - the gap invariant (b) left open. A test-only
    * subclass (below) accepts an InjectState message that reuses the existing protected
    * `updateNodeView`, so a synthetic token-bearing state can be installed without minting a token
    * through a genesis box (which is not possible: the genesis boxes are respectively false-locked,
    * miner-only, and 2-of-N founder-guarded).
    */
  private def isInvalidatedInInstalledPool(view: CurrentView[UtxoState], id: ModifierId): Boolean =
    view.pool.isInvalidated(id)

  it should "(h) install the invalidation into the node view so the rejected id is cached" in {
    implicit val system: ActorSystem = ActorSystem("prefilter-installed-pool")
    implicit val timeout: Timeout = Timeout(20.seconds)
    try {
      val f = fixture(checkRules = false)
      val nvSettings = f.s.copy(directory = createTempDir.getAbsolutePath)
      // Precondition: the fee gate in `process` runs before input resolution and the prefilter, so
      // the end-to-end path only reaches the prefilter because the test config sets this to 0 (the
      // production default is 1000000). Assert it rather than trust it.
      withClue("actor fixture must reach the prefilter: ") {
        nvSettings.nodeSettings.minimalFeeAmount shouldBe 0L
      }
      val tx = preserving(f, f.tokenBox)

      val holder = system.actorOf(Props(new PrefilterTestNodeViewHolder(nvSettings)))
      val submitProbe = TestProbe()
      val eventProbe = TestProbe()

      // Both messages go through the SAME probe: Akka guarantees FIFO only per sender-receiver pair,
      // so sending InjectState with no sender could let the transaction overtake it and hit the
      // missing-UTXO path. InjectState is handled synchronously and, being state-only, publishes no
      // ChangedMempool, so subscribing after it still cannot catch a stray event.
      submitProbe.send(holder, InjectState(f.state))
      system.eventStream.subscribe(eventProbe.ref, classOf[ChangedMempool])

      submitProbe.send(holder, LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None)))
      val outcome = submitProbe.expectMsgType[ProcessingOutcome.Declined](10.seconds)
      declinedByPrefilter(outcome) shouldBe true

      // The install publishes ChangedMempool, and the reader it carries reports the cached id.
      val changed = eventProbe.expectMsgType[ChangedMempool](10.seconds)
      changed.mempool match {
        case mp: ErgoMemPool => mp.isInvalidated(tx.id) shouldBe true
        case other => fail(s"ChangedMempool carried an unexpected reader: $other")
      }

      // And the pool the node actually holds - queried fresh - reports it too.
      val installed = Await.result(
        (holder ? GetDataFromCurrentView[UtxoState, Boolean](v => isInvalidatedInInstalledPool(v, tx.id)))
          .mapTo[Boolean],
        10.seconds
      )
      installed shouldBe true
    } finally {
      Await.ready(system.terminate(), 20.seconds)
    }
  }

  it should "(control) not install (nor emit ChangedMempool) on a declining path that returns the same pool" in {
    implicit val system: ActorSystem = ActorSystem("prefilter-noop-control")
    try {
      val f = fixture(checkRules = false)
      val nvSettings = f.s.copy(directory = createTempDir.getAbsolutePath)
      withClue("actor fixture must reach input resolution: ") {
        nvSettings.nodeSettings.minimalFeeAmount shouldBe 0L
      }

      // A box that is not in the installed state: this takes the pre-existing "not all utxos in
      // place yet" Declined path, which returns `this` unchanged. The reference-inequality guard
      // must then skip the install, so no ChangedMempool is emitted.
      val absentBox = box(BoxValue, Constants.TrueTree, Seq.empty, 9)
      val tx = txSpending(Seq(absentBox), Seq(candidate(BoxValue, Constants.TrueTree, Seq.empty)))

      val holder = system.actorOf(Props(new PrefilterTestNodeViewHolder(nvSettings)))
      val submitProbe = TestProbe()
      val eventProbe = TestProbe()

      // Same probe for both, for the FIFO reason given in (h).
      submitProbe.send(holder, InjectState(f.state))
      system.eventStream.subscribe(eventProbe.ref, classOf[ChangedMempool])

      submitProbe.send(holder, LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None)))
      val outcome = submitProbe.expectMsgType[ProcessingOutcome.Declined](10.seconds)
      // Confirm it is the intended path, not the prefilter or some other decline.
      Option(outcome.e.getMessage).exists(_.contains("not all utxos in place yet")) shouldBe true

      eventProbe.expectNoMessage(2.seconds)
    } finally {
      Await.ready(system.terminate(), 20.seconds)
    }
  }

}

/** Message understood only by the test subclass below. */
private case class InjectState(state: UtxoState)

/**
  * A UtxoNodeViewHolder that additionally installs a supplied state on demand, via the same
  * protected `updateNodeView` the production code uses. This is the whole of the test seam - no
  * production code learns about it.
  */
private class PrefilterTestNodeViewHolder(settings: ErgoSettings)
  extends UtxoNodeViewHolder(settings) {

  private def injecting: Receive = {
    case InjectState(s) => updateNodeView(updatedState = Some(s))
  }

  override def receive: Receive = injecting orElse super.receive
}
