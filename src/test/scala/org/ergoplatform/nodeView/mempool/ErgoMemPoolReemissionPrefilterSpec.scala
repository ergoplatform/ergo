package org.ergoplatform.nodeView.mempool

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.BoxHolder
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

  private def tokenOf(s: ErgoSettings): (Digest32Coll, Long) =
    (Digest32Coll @@ s.chainSettings.reemission.reemissionTokenId.toColl) -> 5L

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

  private val PrefilterReason = "Transaction preserves re-emission tokens"

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

}
