package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.DataInput
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADKey
import scorex.util.ModifierId

class OrderedTxPoolSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants.emptyProverResult
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings

  private final class ReconvergentFixture(
    val a: ErgoTransaction,
    val b: ErgoTransaction,
    val c: ErgoTransaction,
    val d: ErgoTransaction,
    val beforeD: ErgoMemPool,
    val parentOrder: Seq[ModifierId]
  )

  private def deterministicRootId(nonce: Int): BoxId = {
    val bytes = Array.fill[Byte](32)(0)
    bytes(28) = (nonce >>> 24).toByte
    bytes(29) = (nonce >>> 16).toByte
    bytes(30) = (nonce >>> 8).toByte
    bytes(31) = nonce.toByte
    ADKey @@ bytes
  }

  private def outputCandidates(plainCount: Int, plainValue: Long = 4000000L) = {
    val feeProposition = settings.chainSettings.monetary.feeProposition
    IndexedSeq.fill(plainCount)(
      new org.ergoplatform.ErgoBoxCandidate(plainValue, TrueTree, 0)
    ) :+ new org.ergoplatform.ErgoBoxCandidate(1000000L, feeProposition, 0)
  }

  private def buildReconvergentFixture(nonce: Int): ReconvergentFixture = {
    val a = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(nonce), emptyProverResult)),
      outputCandidates(3)
    )
    val b = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(a.outputs(0).id, emptyProverResult)),
      outputCandidates(1, 3000000L)
    )
    val c = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(a.outputs(1).id, emptyProverResult)),
      outputCandidates(1, 3000000L)
    )
    val d = ErgoTransaction(
      IndexedSeq(
        new org.ergoplatform.Input(b.outputs(0).id, emptyProverResult),
        new org.ergoplatform.Input(c.outputs(0).id, emptyProverResult),
        new org.ergoplatform.Input(a.outputs(2).id, emptyProverResult)
      ),
      outputCandidates(1, 9000000L)
    )

    val beforeD = Seq(a, b, c).foldLeft(ErgoMemPool.empty(settings)) {
      case (pool, tx) => pool.put(UnconfirmedTransaction(tx, None))
    }
    val parentIds = d.inputs.flatMap(input => beforeD.pool.outputs.get(input.boxId)).toSet
    val parentOrder = parentIds
      .flatMap(id => beforeD.pool.transactionsRegistry.get(id))
      .toSeq
      .map(_.id)

    new ReconvergentFixture(a, b, c, d, beforeD, parentOrder)
  }

  private def fixtureWithSharedAncestorLast(): ReconvergentFixture = {
    (0 until 4096)
      .iterator
      .map(buildReconvergentFixture)
      .find { fixture =>
        fixture.parentOrder.lastOption.contains(fixture.a.id) &&
        fixture.parentOrder.take(2).toSet == Set(fixture.b.id, fixture.c.id)
      }
      .getOrElse(fail("No deterministic reconvergent fixture found"))
  }

  private def orderedIds(pool: OrderedTxPool): Vector[ModifierId] =
    pool.orderedTransactions.valuesIterator.map(_.id).toVector

  private def assertUniqueAndRegistered(pool: OrderedTxPool, expectedIds: Set[ModifierId]): Unit = {
    val ids = orderedIds(pool)
    ids.toSet shouldBe expectedIds
    ids.distinct.size shouldBe ids.size
    pool.orderedTransactions.size shouldBe expectedIds.size
    pool.transactionsRegistry.keySet shouldBe expectedIds
    pool.transactionsRegistry.foreach { case (id, key) =>
      pool.orderedTransactions.get(key).map(_.id) shouldBe Some(id)
    }
    val transactions = pool.orderedTransactions.valuesIterator.map(_.transaction).toVector
    val expectedOutputs = transactions
      .flatMap(tx => tx.outputs.map(_.id -> tx.id))
      .toMap
    val expectedInputs = transactions
      .flatMap(tx => tx.inputs.map(_.boxId -> tx.id))
      .toMap
    val expectedDataInputReaders = transactions
      .flatMap(tx => tx.dataInputs.map(_.boxId -> tx.id))
      .groupBy(_._1)
      .map { case (boxId, readers) => boxId -> readers.map(_._2).toSet }
    pool.outputs shouldBe expectedOutputs
    pool.inputs shouldBe expectedInputs
    pool.dataInputReaders shouldBe expectedDataInputReaders
  }

  private def withDuplicate(pool: OrderedTxPool, tx: ErgoTransaction): OrderedTxPool = {
    val registeredKey = pool.transactionsRegistry(tx.id)
    val unconfirmed = pool.orderedTransactions(registeredKey)
    val duplicateKey = registeredKey.copy(weight = registeredKey.weight + 1L)

    new OrderedTxPool(
      pool.orderedTransactions.updated(duplicateKey, unconfirmed),
      pool.transactionsRegistry,
      pool.invalidatedTxIds,
      pool.outputs,
      pool.inputs,
      pool.dataInputReaders,
      pool.family
    )(settings)
  }

  private def withoutRegistry(pool: OrderedTxPool, tx: ErgoTransaction): OrderedTxPool = {
    new OrderedTxPool(
      pool.orderedTransactions,
      pool.transactionsRegistry - tx.id,
      pool.invalidatedTxIds,
      pool.outputs,
      pool.inputs,
      pool.dataInputReaders,
      pool.family
    )(settings)
  }

  it should "keep one ordered entry per id when a transaction closes a reconvergent family" in {
    val fixture = fixtureWithSharedAncestorLast()
    val beforeIds = Set(fixture.a.id, fixture.b.id, fixture.c.id)
    val beforeWeights = beforeIds.map { id =>
      id -> fixture.beforeD.pool.transactionsRegistry(id).weight
    }.toMap

    fixture.parentOrder.last shouldBe fixture.a.id
    fixture.parentOrder.take(2).toSet shouldBe Set(fixture.b.id, fixture.c.id)

    val afterD = fixture.beforeD.put(UnconfirmedTransaction(fixture.d, None))
    val expectedIds = beforeIds + fixture.d.id
    val ids = orderedIds(afterD.pool)
    val duplicateCounts = ids.groupBy(identity).mapValues(_.size)
    val orderedKeys = afterD.pool.orderedTransactions.keysIterator
      .map(key => key.id -> key.weight)
      .toVector

    withClue(s"ordered keys=$orderedKeys, duplicate counts=$duplicateCounts") {
      assertUniqueAndRegistered(afterD.pool, expectedIds)
      val afterWeights = afterD.pool.transactionsRegistry.mapValues(_.weight)
      val dWeight = afterWeights(fixture.d.id)
      afterWeights(fixture.b.id) shouldBe beforeWeights(fixture.b.id) + dWeight
      afterWeights(fixture.c.id) shouldBe beforeWeights(fixture.c.id) + dWeight
      afterWeights(fixture.a.id) shouldBe beforeWeights(fixture.a.id) + 3L * dWeight
    }
  }

  it should "track a pooled data-input producer without marking its output spent" in {
    val producer = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5000), emptyProverResult)),
      outputCandidates(1)
    )
    val reader = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5001), emptyProverResult)),
      IndexedSeq(DataInput(producer.outputs.head.id)),
      outputCandidates(1)
    )

    val pool = Seq(producer, reader).foldLeft(ErgoMemPool.empty(settings)) {
      case (current, tx) => current.put(UnconfirmedTransaction(tx, None))
    }

    pool.pool.family.readParentsOf(reader.id) shouldBe Set(producer.id)
    pool.pool.family.readChildrenOf(producer.id) shouldBe Set(reader.id)
    pool.pool.family.dependencyParentsOf(reader.id) shouldBe Set(producer.id)
    pool.pool.inputs should not contain producer.outputs.head.id
  }

  it should "restore a spend edge and family weight when the producer arrives after its child" in {
    val producer = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5100), emptyProverResult)),
      outputCandidates(1)
    )
    val child = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(producer.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )

    val childFirst = OrderedTxPool.empty(settings)
      .put(UnconfirmedTransaction(child, None), child.size)
    childFirst.family.parentsOf(child.id) shouldBe empty

    val restored = childFirst.put(UnconfirmedTransaction(producer, None), producer.size)
    val producerKey = restored.transactionsRegistry(producer.id)
    val childKey = restored.transactionsRegistry(child.id)

    restored.family.parentsOf(child.id) shouldBe Set(producer.id)
    restored.family.childrenOf(producer.id) shouldBe Set(child.id)
    producerKey.weight shouldBe producerKey.feePerFactor + childKey.weight
  }

  it should "restore a read edge without changing family weight when the producer arrives late" in {
    val producer = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5200), emptyProverResult)),
      outputCandidates(1)
    )
    val reader = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5201), emptyProverResult)),
      IndexedSeq(DataInput(producer.outputs.head.id)),
      outputCandidates(1)
    )

    val readerFirst = OrderedTxPool.empty(settings)
      .put(UnconfirmedTransaction(reader, None), reader.size)
    readerFirst.family.readParentsOf(reader.id) shouldBe empty

    val restored = readerFirst.put(UnconfirmedTransaction(producer, None), producer.size)
    val producerKey = restored.transactionsRegistry(producer.id)

    restored.family.readParentsOf(reader.id) shouldBe Set(producer.id)
    restored.family.readChildrenOf(producer.id) shouldBe Set(reader.id)
    restored.family.parentsOf(reader.id) shouldBe empty
    producerKey.weight shouldBe producerKey.feePerFactor
  }

  it should "restore retained spend and read children when a producer is removed and reinserted" in {
    val producer = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5300), emptyProverResult)),
      outputCandidates(2)
    )
    val child = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(producer.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )
    val reader = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5301), emptyProverResult)),
      IndexedSeq(DataInput(producer.outputs(1).id)),
      outputCandidates(1)
    )

    val initial = Seq(producer, child, reader).foldLeft(OrderedTxPool.empty(settings)) {
      case (pool, tx) => pool.put(UnconfirmedTransaction(tx, None), tx.size)
    }
    val removed = initial.remove(producer)
    removed.transactionsRegistry.keySet shouldBe Set(child.id, reader.id)
    removed.family.parentsOf(child.id) shouldBe empty
    removed.family.readParentsOf(reader.id) shouldBe empty

    val restored = removed.put(UnconfirmedTransaction(producer, None), producer.size)
    val producerKey = restored.transactionsRegistry(producer.id)
    val childKey = restored.transactionsRegistry(child.id)

    restored.family.parentsOf(child.id) shouldBe Set(producer.id)
    restored.family.readParentsOf(reader.id) shouldBe Set(producer.id)
    producerKey.weight shouldBe producerKey.feePerFactor + childKey.weight
  }

  it should "restore reconvergent path multiplicity when an ancestor is reinserted" in {
    val fixture = fixtureWithSharedAncestorLast()
    val withDescendant = fixture.beforeD.pool
      .put(UnconfirmedTransaction(fixture.d, None), fixture.d.size)
    val expectedWeight = withDescendant.transactionsRegistry(fixture.a.id).weight

    val removed = withDescendant.remove(fixture.a)
    removed.transactionsRegistry.keySet shouldBe Set(fixture.b.id, fixture.c.id, fixture.d.id)

    val restored = removed.put(UnconfirmedTransaction(fixture.a, None), fixture.a.size)
    restored.family.childrenOf(fixture.a.id) shouldBe Set(
      fixture.b.id,
      fixture.c.id,
      fixture.d.id
    )
    restored.transactionsRegistry(fixture.a.id).weight shouldBe expectedWeight
  }

  it should "restore a reinserted family weight through its retained ancestor" in {
    val grandparent = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5350), emptyProverResult)),
      outputCandidates(1)
    )
    val parent = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(grandparent.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )
    val child = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(parent.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )
    val initial = Seq(grandparent, parent, child).foldLeft(OrderedTxPool.empty(settings)) {
      case (pool, tx) => pool.put(UnconfirmedTransaction(tx, None), tx.size)
    }
    val expectedGrandparentWeight = initial.transactionsRegistry(grandparent.id).weight
    val expectedParentWeight = initial.transactionsRegistry(parent.id).weight

    val restored = initial
      .remove(parent)
      .put(UnconfirmedTransaction(parent, None), parent.size)

    restored.family.parentsOf(parent.id) shouldBe Set(grandparent.id)
    restored.family.parentsOf(child.id) shouldBe Set(parent.id)
    restored.transactionsRegistry(parent.id).weight shouldBe expectedParentWeight
    restored.transactionsRegistry(grandparent.id).weight shouldBe expectedGrandparentWeight
  }

  it should "keep parallel data-input readers distinct across removal" in {
    val producer = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5400), emptyProverResult)),
      outputCandidates(1)
    )
    def reader(nonce: Int): ErgoTransaction = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(nonce), emptyProverResult)),
      IndexedSeq(DataInput(producer.outputs.head.id)),
      outputCandidates(1)
    )
    val reader1 = reader(5401)
    val reader2 = reader(5402)

    val withReaders = Seq(reader1, reader2, producer).foldLeft(OrderedTxPool.empty(settings)) {
      case (pool, tx) => pool.put(UnconfirmedTransaction(tx, None), tx.size)
    }
    withReaders.family.readChildrenOf(producer.id) shouldBe Set(reader1.id, reader2.id)
    withReaders.dataInputReaders(producer.outputs.head.id) shouldBe Set(reader1.id, reader2.id)

    val removed = withReaders.remove(reader1)
    removed.family.readChildrenOf(producer.id) shouldBe Set(reader2.id)
    removed.family.readParentsOf(reader2.id) shouldBe Set(producer.id)
    removed.dataInputReaders(producer.outputs.head.id) shouldBe Set(reader2.id)
  }

  it should "clean data-input edges and indexes when a reader is evicted" in {
    val limitedSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(mempoolCapacity = 2)
    )
    val producer = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5450), emptyProverResult)),
      outputCandidates(1)
    )
    val reader = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5451), emptyProverResult)),
      IndexedSeq(DataInput(producer.outputs.head.id)),
      outputCandidates(1)
    )
    val unrelated = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5452), emptyProverResult)),
      outputCandidates(1)
    )

    val withProducer = OrderedTxPool.empty(limitedSettings)
      .put(UnconfirmedTransaction(producer, None), feeFactor = 1)
    val withReader = withProducer
      .put(UnconfirmedTransaction(reader, None), feeFactor = Int.MaxValue)

    withReader.family.readChildrenOf(producer.id) shouldBe Set(reader.id)
    withReader.dataInputReaders(producer.outputs.head.id) shouldBe Set(reader.id)

    val afterEviction = withReader
      .put(UnconfirmedTransaction(unrelated, None), feeFactor = 1)

    assertUniqueAndRegistered(afterEviction, Set(producer.id, unrelated.id))
    afterEviction.family.readChildrenOf(producer.id) shouldBe empty
    afterEviction.family.readParentsOf(reader.id) shouldBe empty
    afterEviction.dataInputReaders should not contain producer.outputs.head.id
  }

  it should "self-heal duplicate keys without propagating family weight twice" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val originalKey = before.transactionsRegistry(fixture.b.id)
    val ancestorWeight = before.transactionsRegistry(fixture.a.id).weight
    val corrupted = withDuplicate(before, fixture.b)

    orderedIds(corrupted).count(_ == fixture.b.id) shouldBe 2
    val healed = corrupted.put(UnconfirmedTransaction(fixture.b, None), fixture.b.size)

    assertUniqueAndRegistered(healed, Set(fixture.a.id, fixture.b.id, fixture.c.id))
    val healedKeys = healed.orderedTransactions.keysIterator
      .filter(_.id == fixture.b.id)
      .toVector
    healedKeys.map(_.weight) shouldBe Vector(originalKey.weight)
    healedKeys.map(_.created) shouldBe Vector(originalKey.created)
    healed.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight
  }

  it should "heal a duplicated ancestor while propagating a new child" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val ancestorWeight = before.transactionsRegistry(fixture.a.id).weight
    val corrupted = withDuplicate(before, fixture.a)

    orderedIds(corrupted).count(_ == fixture.a.id) shouldBe 2
    val healed = corrupted.put(UnconfirmedTransaction(fixture.d, None), fixture.d.size)

    assertUniqueAndRegistered(
      healed,
      Set(fixture.a.id, fixture.b.id, fixture.c.id, fixture.d.id)
    )
    val dWeight = healed.transactionsRegistry(fixture.d.id).weight
    healed.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight + 3L * dWeight
  }

  it should "purge duplicate keys and subtract family weight once on removal" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val ancestorWeight = before.transactionsRegistry(fixture.a.id).weight
    val childWeight = before.transactionsRegistry(fixture.b.id).weight
    val siblingWeight = before.transactionsRegistry(fixture.c.id).weight
    val corrupted = withDuplicate(before, fixture.b)

    orderedIds(corrupted).count(_ == fixture.b.id) shouldBe 2
    val removed = corrupted.remove(fixture.b)

    assertUniqueAndRegistered(removed, Set(fixture.a.id, fixture.c.id))
    removed.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight - childWeight
    removed.transactionsRegistry(fixture.c.id).weight shouldBe siblingWeight
  }

  it should "purge duplicate keys and subtract family weight once on invalidation" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val ancestorWeight = before.transactionsRegistry(fixture.a.id).weight
    val childWeight = before.transactionsRegistry(fixture.b.id).weight
    val siblingWeight = before.transactionsRegistry(fixture.c.id).weight
    val corrupted = withDuplicate(before, fixture.b)

    orderedIds(corrupted).count(_ == fixture.b.id) shouldBe 2
    val invalidated = corrupted.invalidate(UnconfirmedTransaction(fixture.b, None))

    assertUniqueAndRegistered(invalidated, Set(fixture.a.id, fixture.c.id))
    invalidated.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight - childWeight
    invalidated.transactionsRegistry(fixture.c.id).weight shouldBe siblingWeight
    invalidated.isInvalidated(fixture.b.id) shouldBe true
  }

  it should "purge unregistered orphan keys on removal and invalidation" in {
    val fixture = fixtureWithSharedAncestorLast()
    val orphaned = withoutRegistry(fixture.beforeD.pool, fixture.b)
    val expectedIds = Set(fixture.a.id, fixture.c.id)
    val expectedAncestorWeight = fixture.beforeD.pool.transactionsRegistry(fixture.a.id).weight -
      fixture.beforeD.pool.transactionsRegistry(fixture.b.id).weight

    orphaned.transactionsRegistry should not contain fixture.b.id
    orderedIds(orphaned).count(_ == fixture.b.id) shouldBe 1

    val removed = orphaned.remove(fixture.b)
    assertUniqueAndRegistered(removed, expectedIds)
    removed.transactionsRegistry(fixture.a.id).weight shouldBe expectedAncestorWeight

    val invalidated = orphaned.invalidate(UnconfirmedTransaction(fixture.b, None))
    assertUniqueAndRegistered(invalidated, expectedIds)
    invalidated.transactionsRegistry(fixture.a.id).weight shouldBe expectedAncestorWeight
    invalidated.isInvalidated(fixture.b.id) shouldBe true
  }

  it should "re-register an ordered orphan without propagating its family weight twice" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val orphanKey = before.transactionsRegistry(fixture.b.id)
    val ancestorWeight = before.transactionsRegistry(fixture.a.id).weight
    val orphaned = withoutRegistry(before, fixture.b)

    val healed = orphaned.put(UnconfirmedTransaction(fixture.b, None), fixture.b.size)

    assertUniqueAndRegistered(healed, Set(fixture.a.id, fixture.b.id, fixture.c.id))
    healed.transactionsRegistry(fixture.b.id) shouldBe orphanKey
    healed.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight
  }

  it should "re-register a registry-only transaction without propagating its family weight twice" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val childKey = before.transactionsRegistry(fixture.b.id)
    val ancestorWeight = before.transactionsRegistry(fixture.a.id).weight
    val registryOnly = new OrderedTxPool(
      before.orderedTransactions - childKey,
      before.transactionsRegistry,
      before.invalidatedTxIds,
      before.outputs,
      before.inputs,
      before.dataInputReaders,
      before.family
    )(settings)

    registryOnly.currentTransaction(fixture.b.id) shouldBe None
    val healed = registryOnly.put(UnconfirmedTransaction(fixture.b, None), fixture.b.size)

    assertUniqueAndRegistered(healed, Set(fixture.a.id, fixture.b.id, fixture.c.id))
    healed.transactionsRegistry(fixture.b.id) shouldBe childKey
    healed.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight
  }

  it should "reconcile descendants added while a transaction is registry-only" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val parentKey = before.transactionsRegistry(fixture.b.id)
    val ancestorWeight = before.transactionsRegistry(fixture.a.id).weight
    val registryOnly = new OrderedTxPool(
      before.orderedTransactions - parentKey,
      before.transactionsRegistry,
      before.invalidatedTxIds,
      before.outputs,
      before.inputs,
      before.dataInputReaders,
      before.family
    )(settings)
    val child = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(fixture.b.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )
    val withChild = registryOnly.put(UnconfirmedTransaction(child, None), child.size)

    withChild.family.parentsOf(child.id) shouldBe Set(fixture.b.id)
    withChild.currentTransaction(fixture.b.id) shouldBe None
    withChild.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight

    val healed = withChild.put(UnconfirmedTransaction(fixture.b, None), fixture.b.size)
    val childWeight = healed.transactionsRegistry(child.id).weight

    assertUniqueAndRegistered(
      healed,
      Set(fixture.a.id, fixture.b.id, fixture.c.id, child.id)
    )
    healed.transactionsRegistry(fixture.b.id).weight shouldBe parentKey.weight + childWeight
    healed.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight + childWeight
  }

  it should "restore full weight through a parent reattached while its child is registry-only" in {
    val grandparent = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5600), emptyProverResult)),
      outputCandidates(1)
    )
    val parent = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(grandparent.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )
    val child = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(parent.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )
    val initial = Seq(grandparent, parent, child).foldLeft(OrderedTxPool.empty(settings)) {
      case (pool, tx) => pool.put(UnconfirmedTransaction(tx, None), tx.size)
    }
    val childKey = initial.transactionsRegistry(child.id)
    val expectedParentWeight = initial.transactionsRegistry(parent.id).weight
    val expectedGrandparentWeight = initial.transactionsRegistry(grandparent.id).weight
    val registryOnlyChild = new OrderedTxPool(
      initial.orderedTransactions - childKey,
      initial.transactionsRegistry,
      initial.invalidatedTxIds,
      initial.outputs,
      initial.inputs,
      initial.dataInputReaders,
      initial.family
    )(settings)

    val withReinsertedParent = registryOnlyChild
      .remove(parent)
      .put(UnconfirmedTransaction(parent, None), parent.size)

    withReinsertedParent.family.parentsOf(child.id) shouldBe empty
    withReinsertedParent.transactionsRegistry(parent.id).weight shouldBe (expectedParentWeight - childKey.weight)
    withReinsertedParent.transactionsRegistry(grandparent.id).weight shouldBe (expectedGrandparentWeight - childKey.weight)

    val healed = withReinsertedParent
      .put(UnconfirmedTransaction(child, None), child.size)

    assertUniqueAndRegistered(healed, Set(grandparent.id, parent.id, child.id))
    healed.transactionsRegistry(parent.id).weight shouldBe expectedParentWeight
    healed.transactionsRegistry(grandparent.id).weight shouldBe expectedGrandparentWeight
  }

  it should "restore family weights when registry-only transactions return parent first" in {
    val grandparent = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(deterministicRootId(5700), emptyProverResult)),
      outputCandidates(1)
    )
    val parent = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(grandparent.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )
    val child = ErgoTransaction(
      IndexedSeq(new org.ergoplatform.Input(parent.outputs.head.id, emptyProverResult)),
      outputCandidates(1)
    )
    val initial = Seq(grandparent, parent, child).foldLeft(OrderedTxPool.empty(settings)) {
      case (pool, tx) => pool.put(UnconfirmedTransaction(tx, None), tx.size)
    }
    val parentKey = initial.transactionsRegistry(parent.id)
    val childKey = initial.transactionsRegistry(child.id)
    val expectedGrandparentWeight = initial.transactionsRegistry(grandparent.id).weight
    val expectedParentWeight = parentKey.weight
    val registryOnlyChain = new OrderedTxPool(
      initial.orderedTransactions -- Seq(parentKey, childKey),
      initial.transactionsRegistry,
      initial.invalidatedTxIds,
      initial.outputs,
      initial.inputs,
      initial.dataInputReaders,
      initial.family
    )(settings)

    val healed = registryOnlyChain
      .put(UnconfirmedTransaction(parent, None), parent.size)
      .put(UnconfirmedTransaction(child, None), child.size)

    assertUniqueAndRegistered(healed, Set(grandparent.id, parent.id, child.id))
    healed.transactionsRegistry(parent.id).weight shouldBe expectedParentWeight
    healed.transactionsRegistry(grandparent.id).weight shouldBe expectedGrandparentWeight
  }

  it should "purge a transaction stored under a key different from its registry key" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val actualKey = before.transactionsRegistry(fixture.b.id)
    val ancestorWeight = before.transactionsRegistry(fixture.a.id).weight
    val staleKey = actualKey.copy(weight = actualKey.weight + 1L)
    val broken = new OrderedTxPool(
      before.orderedTransactions,
      before.transactionsRegistry.updated(fixture.b.id, staleKey),
      before.invalidatedTxIds,
      before.outputs,
      before.inputs,
      before.dataInputReaders,
      before.family
    )(settings)

    broken.get(fixture.b.id) shouldBe None
    orderedIds(broken) should contain(fixture.b.id)

    val invalidated = broken.invalidate(UnconfirmedTransaction(fixture.b, None))
    assertUniqueAndRegistered(invalidated, Set(fixture.a.id, fixture.c.id))
    invalidated.transactionsRegistry(fixture.a.id).weight shouldBe ancestorWeight - actualKey.weight
    invalidated.isInvalidated(fixture.b.id) shouldBe true
  }

  it should "heal a stale registry key on an ancestor during family propagation" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val actualKey = before.transactionsRegistry(fixture.a.id)
    val staleKey = actualKey.copy(weight = actualKey.weight + 1L)
    val broken = new OrderedTxPool(
      before.orderedTransactions,
      before.transactionsRegistry.updated(fixture.a.id, staleKey),
      before.invalidatedTxIds,
      before.outputs,
      before.inputs,
      before.dataInputReaders,
      before.family
    )(settings)

    val healed = broken.put(UnconfirmedTransaction(fixture.d, None), fixture.d.size)
    val dWeight = healed.transactionsRegistry(fixture.d.id).weight

    assertUniqueAndRegistered(
      healed,
      Set(fixture.a.id, fixture.b.id, fixture.c.id, fixture.d.id)
    )
    healed.transactionsRegistry(fixture.a.id).weight shouldBe actualKey.weight + 3L * dWeight
  }

  it should "restore a missing ancestor registry entry during family propagation" in {
    val fixture = fixtureWithSharedAncestorLast()
    val before = fixture.beforeD.pool
    val ancestorKey = before.transactionsRegistry(fixture.a.id)
    val broken = withoutRegistry(before, fixture.a)

    val healed = broken.put(UnconfirmedTransaction(fixture.d, None), fixture.d.size)
    val dWeight = healed.transactionsRegistry(fixture.d.id).weight

    assertUniqueAndRegistered(
      healed,
      Set(fixture.a.id, fixture.b.id, fixture.c.id, fixture.d.id)
    )
    healed.transactionsRegistry(fixture.a.id).weight shouldBe ancestorKey.weight + 3L * dWeight
  }
}
