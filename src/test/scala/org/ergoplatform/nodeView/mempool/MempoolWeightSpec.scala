package org.ergoplatform.nodeView.mempool

import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.core.VersionTag
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.LocallyGeneratedModifier
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.{ProcessingOutcome, SortingOption}
import org.ergoplatform.nodeView.mempool.OrderedTxPool.{WeightedTxId, feeForFactor, saturatedAverage, saturatingAdd, saturatingSubtract, scaledFeePerFactor}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.{ADKey, ADDigest}
import scorex.db.LDBVersionedStore

import scala.util.Try

class MempoolWeightSpec extends AnyFlatSpec
  with Matchers
  with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val monetarySettings = settings.chainSettings.monetary

  it should "keep high fee weights at top priority when precision scaling would overflow" in {
    val feeOut = new ErgoBoxCandidate(Long.MaxValue / 1024 + 1, feeProp, creationHeight = 0)
    val tx = ErgoTransaction(IndexedSeq.empty, IndexedSeq(feeOut))

    val weightedTx = OrderedTxPool.weighted(tx, feeFactor = 1)(monetarySettings)

    weightedTx.feePerFactor shouldBe Long.MaxValue
    weightedTx.weight shouldBe Long.MaxValue
  }

  it should "saturate mempool weight arithmetic" in {
    saturatingAdd(Long.MaxValue - 1, 2) shouldBe Long.MaxValue
    saturatedAverage(Seq(Long.MaxValue, Long.MaxValue)) shouldBe Long.MaxValue
    feeForFactor(Long.MaxValue, 2048) shouldBe Long.MaxValue
  }

  it should "saturate fee histogram totals" in {
    val now = System.currentTimeMillis()
    val tx = ErgoTransaction(IndexedSeq.empty, IndexedSeq(new ErgoBoxCandidate(1L, feeProp, creationHeight = 0)))
    val wtx = WeightedTxId(tx.id, Long.MaxValue, Long.MaxValue, now)

    val stats = MemPoolStatistics(
      startMeasurement = now,
      snapTime = now,
      histogram = FeeHistogramBin(0, Long.MaxValue - 1) :: MemPoolStatistics.defaultPoolHistogram.tail
    ).add(now, wtx)
    stats.histogram.head.totalFee shouldBe Long.MaxValue

    val histogram = HistogramStats.getFeeHistogram(now, nBins = 1, maxWaitTimeMsec = 60000L, Seq(wtx, wtx))
    histogram.head.totalFee shouldBe Long.MaxValue
  }

  it should "match BigInt reference arithmetic at signed and scaling boundaries" in {
    def clamp(value: BigInt): Long =
      value.max(BigInt(Long.MinValue)).min(BigInt(Long.MaxValue)).toLong
    def scale(value: Long, multiplier: Long, divisor: Long): Long =
      if (value <= 0) 0L else clamp(BigInt(value) * BigInt(multiplier) / BigInt(divisor))

    val additions: Seq[(Long, Long)] = Seq(
      (Long.MaxValue, 1L),
      (Long.MinValue, -1L),
      (Long.MaxValue, Long.MaxValue),
      (Long.MinValue, Long.MinValue),
      (Long.MinValue, Long.MaxValue),
      (Long.MaxValue - 1, 1L),
      (Long.MinValue + 1, -1L),
      (0L, 0L),
      (-1L, 1L)
    )
    additions.foreach { case (left, right) =>
      saturatingAdd(left, right) shouldBe clamp(BigInt(left) + BigInt(right))
    }

    val subtractions: Seq[(Long, Long)] = Seq(
      (Long.MaxValue, Long.MinValue),
      (Long.MinValue, Long.MaxValue),
      (Long.MinValue, Long.MinValue),
      (Long.MaxValue, Long.MaxValue),
      (-Long.MaxValue, Long.MinValue),
      (0L, Long.MinValue),
      (0L, Long.MaxValue),
      (Long.MinValue, -1L),
      (Long.MaxValue, 1L),
      (0L, 0L)
    )
    subtractions.foreach { case (left, right) =>
      saturatingSubtract(left, right) shouldBe clamp(BigInt(left) - BigInt(right))
    }

    val scaling: Seq[(Long, Int)] = Seq(
      (Long.MinValue, 1),
      (-1L, 1),
      (0L, 1),
      (1L, 1),
      (1L, Int.MaxValue),
      (1023L, 1),
      (1024L, 1),
      (1025L, 1),
      (Long.MaxValue / 1024, 1),
      (Long.MaxValue / 1024 + 1, 1),
      (Long.MaxValue, 1),
      (Long.MaxValue, 1024),
      (Long.MaxValue, 2048),
      ((Long.MaxValue / 1025L) * 1024L + 1023L, 1025),
      (Long.MaxValue, Int.MaxValue),
      (Int.MaxValue.toLong - 1, Int.MaxValue),
      (1023L, Int.MaxValue)
    )
    scaling.foreach { case (value, factor) =>
      scaledFeePerFactor(value, factor) shouldBe scale(value, 1024L, factor)
      feeForFactor(value, factor) shouldBe scale(value, factor, 1024L)
    }
    Seq(0, -1).foreach { factor =>
      intercept[IllegalArgumentException](scaledFeePerFactor(1L, factor))
      intercept[IllegalArgumentException](feeForFactor(1L, factor))
    }
  }

  it should "preserve bounded arithmetic through actual mempool callers and signed ordering" in {
    val multiFee = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(new Input(ADKey @@ Array.fill(32)(5.toByte), emptyProverResult)),
      IndexedSeq.fill(2)(new ErgoBoxCandidate(Long.MaxValue, feeProp, creationHeight = 0))
    ), None).withCost(validationCost)
    val (parentPool, parentOutcome) = ErgoMemPool.empty(rbfSettings).process(multiFee, policyState)
    parentOutcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    parentPool.pool.transactionsRegistry(multiFee.id).weight shouldBe Long.MaxValue

    val child = childOf(multiFee)
    val (familyPool, childOutcome) = parentPool.process(child, policyState)
    childOutcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    familyPool.getAll.map(_.id).toSet shouldBe Set(multiFee.id, child.id)
    familyPool.pool.transactionsRegistry(multiFee.id).weight shouldBe Long.MaxValue

    val now = System.currentTimeMillis()
    val statistics = MemPoolStatistics(
      startMeasurement = now - 1000L,
      takenTxns = 1,
      snapTime = now,
      histogram = FeeHistogramBin(1, Long.MaxValue) :: MemPoolStatistics.defaultPoolHistogram.tail
    )
    val feePool = new ErgoMemPool(OrderedTxPool.empty(rbfSettings), statistics, SortingOption.FeePerCycle)(rbfSettings)
    feePool.getRecommendedFee(expectedWaitTimeMinutes = 1, txSize = 2048) shouldBe Long.MaxValue

    val small = feeTransaction(Seq(6.toByte), 1L)
    val orderedSmall = OrderedTxPool.empty(rbfSettings).put(small, validationCost)
    orderedSmall.transactionsRegistry(small.id).weight shouldBe unitWeight
    (now > statistics.startMeasurement) shouldBe true
    val waitPool = new ErgoMemPool(orderedSmall, statistics, SortingOption.FeePerCycle)(rbfSettings)
    waitPool.getExpectedWaitTime(txFee = Long.MaxValue, txSize = Int.MaxValue) shouldBe 0L

    val zeroA = feeTransaction(Seq(8.toByte), 1L)
    val zeroB = feeTransaction(Seq(10.toByte), 1L)
    val samples = Seq(
      feeTransaction(Seq(7.toByte), 1L) -> Long.MaxValue,
      zeroA -> 0L,
      feeTransaction(Seq(9.toByte), 1L) -> Long.MinValue,
      zeroB -> 0L
    )
    samples.map(_._1.id).distinct.size shouldBe 4
    val ordered = samples.foldLeft(OrderedTxPool.empty(rbfSettings).orderedTransactions) {
      case (entries, (tx, weight)) => entries.updated(WeightedTxId(tx.id, weight, weight, 0L), tx)
    }
    ordered.keysIterator.map(_.weight).toSeq shouldBe Seq(Long.MaxValue, 0L, 0L, Long.MinValue)
    ordered.keysIterator.filter(_.weight == 0).map(_.id).toSeq shouldBe Seq(zeroA.id, zeroB.id).sorted
    ordered.foreach { case (key, tx) => key.id shouldBe tx.id }

    val grandparent = feeTransaction(Seq(11.toByte), 1L)
    val parent = childOf(grandparent)
    val signedChild = childOf(parent)
    val removals: Seq[(String, OrderedTxPool => OrderedTxPool)] = Seq(
      "remove" -> ((pool: OrderedTxPool) => pool.remove(signedChild.transaction)),
      "invalidate" -> ((pool: OrderedTxPool) => pool.invalidate(signedChild))
    )
    removals.foreach { case (label, remove) =>
      withClue(label) {
        val before = signedFamilyPool(grandparent, parent, signedChild)
        before.orderedTransactions.size shouldBe 3
        before.transactionsRegistry(grandparent.id).weight shouldBe -Long.MaxValue
        before.transactionsRegistry(parent.id).weight shouldBe -Long.MaxValue
        before.transactionsRegistry(signedChild.id).weight shouldBe Long.MinValue
        val after = remove(before)
        after.orderedTransactions.valuesIterator.map(_.id).toSet shouldBe Set(grandparent.id, parent.id)
        after.transactionsRegistry(grandparent.id).weight shouldBe 1L
        after.transactionsRegistry(parent.id).weight shouldBe 1L
      }
    }
  }

  private val validationCost = 1000
  private val feeUnit = 1000000L
  private val unitWeight = feeUnit * 1024L / validationCost
  private val rbfSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
    mempoolSorting = SortingOption.FeePerCycle,
    mempoolCapacity = 10
  ))

  // Exercise process/RBF policy without claiming UTXO, script or node validation.
  private final class PolicyOnlyState extends ErgoState[PolicyOnlyState] {
    private def unsupported: Nothing = throw new UnsupportedOperationException("No state operations in this fixture")
    override def rootDigest: ADDigest = unsupported
    override def version: VersionTag = unsupported
    override lazy val store: LDBVersionedStore = unsupported
    override protected def ergoSettings: ErgoSettings = rbfSettings
    override def applyModifier(mod: BlockSection, estimatedTip: Option[Int])
                              (generate: LocallyGeneratedModifier => Unit): Try[PolicyOnlyState] = unsupported
    override def rollbackTo(version: VersionTag): Try[PolicyOnlyState] = unsupported
    override def rollbackVersions: Iterable[VersionTag] = Iterable.empty
  }

  private val policyState = new PolicyOnlyState

  private def feeTransaction(inputSeeds: Seq[Byte], feeUnits: Long): UnconfirmedTransaction = {
    val tx = ErgoTransaction(
      inputSeeds.map(seed => new Input(ADKey @@ Array.fill(32)(seed), emptyProverResult)).toIndexedSeq,
      IndexedSeq(new ErgoBoxCandidate(feeUnits * feeUnit, feeProp, creationHeight = 0))
    )
    UnconfirmedTransaction(tx, None).withCost(validationCost)
  }

  private def childOf(parent: UnconfirmedTransaction): UnconfirmedTransaction =
    UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(new Input(parent.transaction.outputs.head.id, emptyProverResult)),
      IndexedSeq(new ErgoBoxCandidate(feeUnit, feeProp, creationHeight = 0))
    ), None).withCost(validationCost)

  private def signedFamilyPool(grandparent: UnconfirmedTransaction,
                               parent: UnconfirmedTransaction,
                               child: UnconfirmedTransaction): OrderedTxPool = {
    val empty = OrderedTxPool.empty(rbfSettings)
    val entries = Seq[(UnconfirmedTransaction, Long)](
      grandparent -> -Long.MaxValue,
      parent -> -Long.MaxValue,
      child -> Long.MinValue
    ).map { case (tx, weight) => WeightedTxId(tx.id, weight, unitWeight, 0L) -> tx }
    new OrderedTxPool(
      entries.foldLeft(empty.orderedTransactions) { case (pool, (key, tx)) => pool.updated(key, tx) },
      entries.foldLeft(empty.transactionsRegistry) { case (registry, (key, tx)) => registry.updated(tx.id, key) },
      empty.invalidatedTxIds,
      entries.foldLeft(empty.outputs) { case (outputs, (key, tx)) => outputs ++ tx.transaction.outputs.map(_.id -> key) },
      entries.foldLeft(empty.inputs) { case (inputs, (key, tx)) => inputs ++ tx.transaction.inputs.map(_.boxId -> key) }
    )(rbfSettings)
  }

  private def directConflictPool(): (ErgoMemPool, Seq[UnconfirmedTransaction], UnconfirmedTransaction) = {
    (feeUnit >= rbfSettings.nodeSettings.minimalFeeAmount) shouldBe true
    val conflicts = Seq(1.toByte -> 10L, 2.toByte -> 10L, 3.toByte -> 100L)
      .map { case (seed, fee) => feeTransaction(Seq(seed), fee) }
    val sentinel = feeTransaction(Seq(4.toByte), 1L)
    conflicts.map(_.id).distinct.size shouldBe 3
    val before = (conflicts :+ sentinel).foldLeft(ErgoMemPool.empty(rbfSettings)) { case (pool, tx) =>
      val (next, outcome) = pool.process(tx, policyState)
      outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
      next
    }
    before.size shouldBe 4
    val weights = conflicts.map(tx => before.pool.transactionsRegistry(tx.id).weight)
    weights shouldBe Seq(10L, 10L, 100L).map(_ * unitWeight)
    saturatedAverage(weights) shouldBe 40L * unitWeight
    (before.size < rbfSettings.nodeSettings.mempoolCapacity) shouldBe true
    (before, conflicts, sentinel)
  }

  it should "accept an RBF winner above the mean when two conflicts have equal weights" in {
    val (before, conflicts, sentinel) = directConflictPool()
    val candidate = feeTransaction(Seq(1.toByte, 2.toByte, 3.toByte), 50L)
    before.pool.canAccept(candidate) shouldBe true
    OrderedTxPool.weighted(candidate, validationCost)(monetarySettings).weight shouldBe 50L * unitWeight

    val (after, outcome) = before.process(candidate, policyState)

    outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    after.getAll.map(_.id).toSet shouldBe Set(candidate.id, sentinel.id)
    before.getAll.map(_.id).toSet shouldBe (conflicts :+ sentinel).map(_.id).toSet
  }

  it should "reject an RBF candidate below the mean when two conflicts have equal weights" in {
    val (before, conflicts, sentinel) = directConflictPool()
    val candidate = feeTransaction(Seq(1.toByte, 2.toByte, 3.toByte), 38L)
    before.pool.canAccept(candidate) shouldBe true
    OrderedTxPool.weighted(candidate, validationCost)(monetarySettings).weight shouldBe 38L * unitWeight

    val (after, outcome) = before.process(candidate, policyState)

    outcome.isInstanceOf[ProcessingOutcome.DoubleSpendingLoser] shouldBe true
    after.getAll.map(_.id).toSet shouldBe (conflicts :+ sentinel).map(_.id).toSet
    after.contains(candidate.id) shouldBe false
  }
}
