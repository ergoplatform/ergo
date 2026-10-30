package org.ergoplatform.nodeView.mempool

import org.ergoplatform.{DataInput, ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.{
  ProcessingOutcome,
  SortingOption
}
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.ergoplatform.wallet.utils.TestFileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.bytesToId
import sigma.Colls
import sigma.interpreter.ProverResult

import scala.util.Try

/**
  * Regression specification for #2724. Unsafe evictions are declined when the
  * proposed pool no longer supplies inputs that were available at validation.
  * Fixtures use real UTXO validation and admit every ancestor through process().
  */
class ErgoMemPoolInputProvidersSpec extends AnyFlatSpec with Matchers with TestFileUtils {

  private val BOX_VALUE: Long = 10L * CoinsInOneErgo
  private val LOW_FEE: Long   = 1000000L
  private val HIGH_FEE: Long  = 100000000L

  private case class Fixture(
    settings: ErgoSettings,
    state: UtxoState,
    confirmed: IndexedSeq[ErgoBox]
  ) {
    def emptyPool: ErgoMemPool = ErgoMemPool.empty(settings)
  }

  private def withFixture(capacity: Int = 8)(test: Fixture => Unit): Unit = {
    val base = ErgoNodeTestConstants.settings
    val settings = base.copy(nodeSettings = base.nodeSettings.copy(
      mempoolCapacity = capacity,
      mempoolSorting  = SortingOption.FeePerByte
    )
    )
    val confirmed = (0 until 4).map { index =>
      new ErgoBox(
        BOX_VALUE,
        Constants.TrueTree,
        Colls.emptyColl,
        Map.empty,
        bytesToId(Array.fill(32)(1.toByte)),
        index.toShort,
        creationHeight = 0
      )
    }.toIndexedSeq
    val state = UtxoState.fromBoxHolder(
      BoxHolder(confirmed),
      None,
      createTempDir,
      settings,
      settings.launchParameters
    )
    try test(Fixture(settings, state, confirmed))
    finally state.closeStorage()
  }

  private def transaction(
    f: Fixture,
    inputs: Seq[ErgoBox],
    fee: Long,
    dataInputs: Seq[ErgoBox] = Seq.empty
  ): ErgoTransaction =
    ErgoTransaction(
      inputs.map(box => Input(box.id, ProverResult.empty)).toIndexedSeq,
      dataInputs.map(box => DataInput(box.id)).toIndexedSeq,
      IndexedSeq(
        new ErgoBoxCandidate(inputs.map(_.value).sum - fee, Constants.TrueTree, 0),
        new ErgoBoxCandidate(fee, f.settings.chainSettings.monetary.feeProposition, 0)
      )
    )

  private def admit(f: Fixture, pool: ErgoMemPool, tx: ErgoTransaction): ErgoMemPool = {
    val (after, outcome) = pool.process(UnconfirmedTransaction(tx, None), f.state)
    outcome shouldBe a[ProcessingOutcome.Accepted]
    after.contains(tx.id) shouldBe true
    after
  }

  private def validation(f: Fixture, pool: ErgoMemPool, tx: ErgoTransaction): Try[Int] =
    f.state
      .withMempool(pool)
      .validateWithCost(
        tx,
        f.state.stateContext.simplifiedUpcoming(),
        f.settings.nodeSettings.maxTransactionCost,
        None
      )

  private def expectDeclined(
    f: Fixture,
    before: ErgoMemPool,
    incoming: ErgoTransaction,
    provider: ErgoTransaction,
    dependent: ErgoTransaction,
    expectedMissingBox: Option[ErgoBox] = None
  ): Unit = {
    before.contains(provider.id) shouldBe true
    f.state.boxById(provider.outputs.head.id) shouldBe None
    validation(f, before, incoming).isSuccess shouldBe true
    validation(f, before, dependent).isSuccess shouldBe true

    val (after, outcome) = before.process(UnconfirmedTransaction(incoming, None), f.state)
    withClue(
      s"Provider retained: ${after.contains(provider.id)}; " +
      s"dependent retained: ${after.contains(dependent.id)}; " +
      s"incoming validation after admission: ${validation(f, after, incoming)}; " +
      s"dependent validation after admission: ${validation(f, after, dependent)}. "
    ) {
      outcome shouldBe a[ProcessingOutcome.Declined]
    }
    expectedMissingBox.foreach { box =>
      val actual = outcome.asInstanceOf[ProcessingOutcome.Declined].e.getMessage
      actual shouldBe ErgoMemPool.availabilityDeclineReason(ErgoMemPool.Missing(box.id)).get
    }
    after.getAll shouldBe before.getAll
    after.pool.transactionsRegistry shouldBe before.pool.transactionsRegistry
    after.pool.inputs shouldBe before.pool.inputs
    after.pool.outputs shouldBe before.pool.outputs
    after.stats shouldBe before.stats
    after.isInvalidated(incoming.id) shouldBe before.isInvalidated(incoming.id)
  }

  "Mempool admission" should "decline RBF that removes its own data-input provider" in {
    withFixture() { f =>
      val provider = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val before   = admit(f, f.emptyPool, provider)
      val incoming = transaction(
        f,
        Seq(f.confirmed(0)),
        HIGH_FEE,
        Seq(provider.outputs.head)
      )

      expectDeclined(f, before, incoming, provider, incoming, Some(provider.outputs.head))
    }
  }

  it should "decline capacity eviction that removes its own data-input provider" in {
    withFixture(capacity = 1) { f =>
      val provider = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val before   = admit(f, f.emptyPool, provider)
      val incoming = transaction(
        f,
        Seq(f.confirmed(1)),
        HIGH_FEE,
        Seq(provider.outputs.head)
      )

      incoming.inputs.head.boxId should not be provider.inputs.head.boxId
      expectDeclined(f, before, incoming, provider, incoming)
    }
  }

  it should "decline RBF that removes a retained ancestor's regular-input provider" in {
    withFixture() { f =>
      val provider     = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val withProvider = admit(f, f.emptyPool, provider)
      val ancestor     = transaction(f, Seq(provider.outputs.head), LOW_FEE)
      val before       = admit(f, withProvider, ancestor)
      val incoming = transaction(
        f,
        Seq(f.confirmed(0), ancestor.outputs.head),
        HIGH_FEE
      )

      expectDeclined(f, before, incoming, provider, ancestor)
    }
  }

  it should "decline RBF that removes a retained ancestor's data-input provider" in {
    withFixture() { f =>
      val provider     = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val withProvider = admit(f, f.emptyPool, provider)
      val ancestor = transaction(
        f,
        Seq(f.confirmed(1)),
        LOW_FEE,
        Seq(provider.outputs.head)
      )
      val before = admit(f, withProvider, ancestor)
      val incoming = transaction(
        f,
        Seq(f.confirmed(0), ancestor.outputs.head),
        HIGH_FEE
      )

      expectDeclined(f, before, incoming, provider, ancestor)
    }
  }

  it should "decline RBF that breaks the ancestry of a data-input provider" in {
    withFixture() { f =>
      val provider     = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val withProvider = admit(f, f.emptyPool, provider)
      val ancestor     = transaction(f, Seq(provider.outputs.head), LOW_FEE)
      val withAncestor = admit(f, withProvider, ancestor)
      val descendant   = transaction(f, Seq(ancestor.outputs.head), LOW_FEE)
      val before       = admit(f, withAncestor, descendant)
      val incoming = transaction(
        f,
        Seq(f.confirmed(0)),
        HIGH_FEE,
        Seq(descendant.outputs.head)
      )

      // All direct inputs still resolve after RBF; the missing box is two ancestors away.
      expectDeclined(f, before, incoming, provider, ancestor)
    }
  }

  it should "decline capacity eviction that breaks a retained ancestor's data input" in {
    withFixture(capacity = 2) { f =>
      val provider     = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val withProvider = admit(f, f.emptyPool, provider)
      val ancestor = transaction(
        f,
        Seq(f.confirmed(1)),
        2L * LOW_FEE,
        Seq(provider.outputs.head)
      )
      val before   = admit(f, withProvider, ancestor)
      val incoming = transaction(f, Seq(ancestor.outputs.head), HIGH_FEE)

      expectDeclined(f, before, incoming, provider, ancestor)
    }
  }

  it should "accept RBF when its data input is available in the confirmed UTXO state" in {
    withFixture() { f =>
      val provider = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val before   = admit(f, f.emptyPool, provider)
      val incoming = transaction(
        f,
        Seq(f.confirmed(0)),
        HIGH_FEE,
        Seq(f.confirmed(2))
      )
      f.state.boxById(f.confirmed(2).id).isDefined shouldBe true

      val after = admit(f, before, incoming)

      after.getAll.map(_.id) shouldBe Seq(incoming.id)
      validation(f, after, incoming).isSuccess shouldBe true
    }
  }

  it should "accept capacity eviction when a former pool output is now confirmed" in {
    withFixture(capacity = 1) { f =>
      val provider       = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val before         = admit(f, f.emptyPool, provider)
      val formerlyPooled = provider.outputs.head
      val incoming       = transaction(
        f,
        Seq(f.confirmed(1)),
        HIGH_FEE,
        Seq(formerlyPooled)
      )

      before.pool.outputs.contains(formerlyPooled.id) shouldBe true
      f.state.boxById(formerlyPooled.id) shouldBe None
      val (withoutConfirmation, missingOutcome) =
        before.process(UnconfirmedTransaction(incoming, None), f.state)
      missingOutcome shouldBe a[ProcessingOutcome.Declined]
      (withoutConfirmation eq before) shouldBe true

      // Model a stale pool against a later UTXO view: P's spent input is gone,
      // while the output P supplied is now confirmed.
      val confirmedState = UtxoState.fromBoxHolder(
        BoxHolder(f.confirmed.drop(1) :+ formerlyPooled),
        None,
        createTempDir,
        f.settings,
        f.settings.launchParameters
      )
      try {
        confirmedState.boxById(f.confirmed(0).id) shouldBe None
        confirmedState.boxById(formerlyPooled.id).isDefined shouldBe true
        confirmedState.withMempool(before).validateWithCost(
          incoming,
          confirmedState.stateContext.simplifiedUpcoming(),
          f.settings.nodeSettings.maxTransactionCost,
          None
        ).isSuccess shouldBe true

        val (after, outcome) = before.process(
          UnconfirmedTransaction(incoming, None),
          confirmedState
        )
        outcome shouldBe a[ProcessingOutcome.Accepted]
        after.getAll.map(_.id) shouldBe Seq(incoming.id)
        confirmedState.withMempool(after).validateWithCost(
          incoming,
          confirmedState.stateContext.simplifiedUpcoming(),
          f.settings.nodeSettings.maxTransactionCost,
          None
        ).isSuccess shouldBe true
      } finally confirmedState.closeStorage()
    }
  }

  it should "accept an independent higher-fee RBF replacement" in {
    withFixture() { f =>
      val provider = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val before   = admit(f, f.emptyPool, provider)
      val incoming = transaction(f, Seq(f.confirmed(0)), HIGH_FEE)

      val after = admit(f, before, incoming)

      after.getAll.map(_.id) shouldBe Seq(incoming.id)
      validation(f, after, incoming).isSuccess shouldBe true
    }
  }

  it should "accept independent capacity eviction" in {
    withFixture(capacity = 1) { f =>
      val provider = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val before   = admit(f, f.emptyPool, provider)
      val incoming = transaction(f, Seq(f.confirmed(1)), HIGH_FEE)

      val after = admit(f, before, incoming)

      after.getAll.map(_.id) shouldBe Seq(incoming.id)
      validation(f, after, incoming).isSuccess shouldBe true
    }
  }

  it should "decline a child evicted by its own insertion and accept an independent control" in {
    withFixture(capacity = 2) { f =>
      val provider = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val other    = transaction(f, Seq(f.confirmed(1)), HIGH_FEE * 8)
      val before   = admit(f, admit(f, f.emptyPool, provider), other)

      val control = transaction(f, Seq(f.confirmed(2)), HIGH_FEE * 2)
      val (controlAfter, controlOutcome) =
        before.process(UnconfirmedTransaction(control, None), f.state)
      controlOutcome shouldBe a[ProcessingOutcome.Accepted]
      controlAfter.contains(control.id) shouldBe true
      controlAfter.contains(provider.id) shouldBe false

      val incoming = transaction(f, Seq(provider.outputs.head), HIGH_FEE)
      validation(f, before, incoming).isSuccess shouldBe true
      val proposed = before.pool.put(UnconfirmedTransaction(incoming, None), incoming.size)
      proposed.get(provider.id).isDefined shouldBe true
      proposed.get(other.id).isDefined shouldBe true
      proposed.get(incoming.id) shouldBe None

      val (after, outcome) = before.process(UnconfirmedTransaction(incoming, None), f.state)
      withClue("Incoming retained: " + after.contains(incoming.id) + "; ") {
        outcome shouldBe a[ProcessingOutcome.Declined]
      }
      after.getAll shouldBe before.getAll
      after.pool.transactionsRegistry shouldBe before.pool.transactionsRegistry
      after.pool.inputs shouldBe before.pool.inputs
      after.pool.outputs shouldBe before.pool.outputs
      after.stats shouldBe before.stats
    }
  }

  it should "bound distinct unconfirmed producers without counting confirmed input leaves" in {
    withFixture() { f =>
      val provider = transaction(f, Seq(f.confirmed(0)), LOW_FEE)
      val pool = admit(f, f.emptyPool, provider)
      val dependent = transaction(f, Seq(provider.outputs.head), HIGH_FEE)
      val stoppedClock: () => Long = () => 0L

      pool.inputsRemainAvailable(
        dependent, pool.pool, f.state,
        maxProducerExpansions = 1, maxScanNanos = 1L, nowNanos = stoppedClock
      ) shouldBe ErgoMemPool.Available
      pool.inputsRemainAvailable(
        dependent, pool.pool, f.state,
        maxProducerExpansions = 0, maxScanNanos = 1L, nowNanos = stoppedClock
      ) shouldBe ErgoMemPool.BudgetExceeded

      val confirmedOnly = transaction(
        f, Seq(f.confirmed(0), f.confirmed(1)), HIGH_FEE,
        dataInputs = Seq(f.confirmed(2), f.confirmed(3))
      )
      val emptyPool = f.emptyPool
      emptyPool.inputsRemainAvailable(
        confirmedOnly, emptyPool.pool, f.state,
        maxProducerExpansions = 0, maxScanNanos = 1L, nowNanos = stoppedClock
      ) shouldBe ErgoMemPool.Available
    }
  }

  it should "bound all enqueued ancestry box references independently of the clock" in {
    withFixture() { f =>
      val stoppedClock: () => Long = () => 0L
      val confirmedOnly = transaction(
        f, Seq(f.confirmed(0), f.confirmed(1)), HIGH_FEE,
        dataInputs = Seq(f.confirmed(2), f.confirmed(3))
      )
      val emptyPool = f.emptyPool
      emptyPool.inputsRemainAvailable(
        confirmedOnly, emptyPool.pool, f.state,
        maxProducerExpansions = 0, maxScanNanos = 1L,
        nowNanos = stoppedClock, maxBoxReferences = 4
      ) shouldBe ErgoMemPool.Available
      emptyPool.inputsRemainAvailable(
        confirmedOnly, emptyPool.pool, f.state,
        maxProducerExpansions = 0, maxScanNanos = 1L,
        nowNanos = stoppedClock, maxBoxReferences = 3
      ) shouldBe ErgoMemPool.BudgetExceeded

      val repeated = transaction(
        f, Seq(f.confirmed(0), f.confirmed(1)), HIGH_FEE,
        dataInputs = Seq(f.confirmed(0), f.confirmed(1))
      )
      emptyPool.inputsRemainAvailable(
        repeated, emptyPool.pool, f.state,
        maxProducerExpansions = 0, maxScanNanos = 1L,
        nowNanos = stoppedClock, maxBoxReferences = 3
      ) shouldBe ErgoMemPool.BudgetExceeded

      val provider = transaction(f, Seq(f.confirmed(0), f.confirmed(1)), LOW_FEE)
      val pool = admit(f, emptyPool, provider)
      val dependent = transaction(f, Seq(provider.outputs.head), HIGH_FEE)
      pool.inputsRemainAvailable(
        dependent, pool.pool, f.state,
        maxProducerExpansions = 1, maxScanNanos = 1L,
        nowNanos = stoppedClock, maxBoxReferences = 3
      ) shouldBe ErgoMemPool.Available
      pool.inputsRemainAvailable(
        dependent, pool.pool, f.state,
        maxProducerExpansions = 1, maxScanNanos = 1L,
        nowNanos = stoppedClock, maxBoxReferences = 2
      ) shouldBe ErgoMemPool.BudgetExceeded
    }
  }

  it should "decline when the ancestry deadline expires during initial enqueue" in {
    withFixture() { f =>
      val confirmedOnly = transaction(
        f, Seq(f.confirmed(0), f.confirmed(1)), HIGH_FEE,
        dataInputs = Seq(f.confirmed(2), f.confirmed(3))
      )
      val pool = f.emptyPool
      val stoppedClock: () => Long = () => 0L
      pool.inputsRemainAvailable(
        confirmedOnly, pool.pool, f.state,
        maxProducerExpansions = 0, maxScanNanos = 2L, nowNanos = stoppedClock
      ) shouldBe ErgoMemPool.Available

      var tick = 0L
      val advancingClock: () => Long = () => {
        tick += 1
        tick
      }
      pool.inputsRemainAvailable(
        confirmedOnly, pool.pool, f.state,
        maxProducerExpansions = 0, maxScanNanos = 2L, nowNanos = advancingClock
      ) shouldBe ErgoMemPool.BudgetExceeded
      tick shouldBe 3L
    }
  }

  it should "decline when the ancestry deadline expires during producer enqueue" in {
    withFixture() { f =>
      val provider = transaction(f, Seq(f.confirmed(0), f.confirmed(1)), LOW_FEE)
      val pool = admit(f, f.emptyPool, provider)
      val dependent = transaction(f, Seq(provider.outputs.head), HIGH_FEE)
      val stoppedClock: () => Long = () => 0L
      pool.inputsRemainAvailable(
        dependent, pool.pool, f.state,
        maxProducerExpansions = 1, maxScanNanos = 4L, nowNanos = stoppedClock
      ) shouldBe ErgoMemPool.Available

      var tick = 0L
      val advancingClock: () => Long = () => {
        tick += 1
        tick
      }
      pool.inputsRemainAvailable(
        dependent, pool.pool, f.state,
        maxProducerExpansions = 1, maxScanNanos = 4L, nowNanos = advancingClock
      ) shouldBe ErgoMemPool.BudgetExceeded
      tick shouldBe 5L
    }
  }

  it should "name only a proven missing input box in the decline detail" in {
    withFixture() { f =>
      val boxId = f.confirmed(0).id
      val encoded = Algos.encode(boxId)
      val missing = ErgoMemPool.availabilityDeclineReason(ErgoMemPool.Missing(boxId)).get
      missing should include(encoded)
      missing should include("available at validation")

      val budget = ErgoMemPool.availabilityDeclineReason(ErgoMemPool.BudgetExceeded).get
      budget should include("budget")
      budget should not include encoded

      val unresolved = ErgoMemPool.availabilityDeclineReason(ErgoMemPool.UnresolvedProducer).get
      unresolved should include("could not be resolved")
      unresolved should not include encoded
      ErgoMemPool.availabilityDeclineReason(ErgoMemPool.Available) shouldBe None
    }
  }
}
