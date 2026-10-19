package org.ergoplatform.nodeView.state

import org.ergoplatform.{CriticalSystemException, DataInput, Input}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.CheckpointSettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, RandomWrapper}
import org.ergoplatform.core._
import scorex.crypto.authds.ADDigest
import scorex.db.LDBVersionedStore
import sigma.interpreter.ProverResult

import java.nio.file.Files

class DigestStateSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private val emptyVersion: VersionTag = bytesToVersion(Array.fill(32)(0: Byte))
  private val emptyAdDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  property("recover a valid checkpoint and reopen it") {
    val (us, bh) = createUtxoState(settings)
    try {
      val block = validFullBlock(parentOpt = None, us, bh)
      val checkpoint = block.header
      val checkpointContext = us.stateContext.appendFullBlock(block).get
      val dir = createTempDir

      def checkCheckpoint(state: DigestState): Unit = {
        state.version shouldEqual checkpoint.id
        state.rootDigest shouldEqual checkpoint.stateRoot
        state.stateContext.lastHeaderOpt.map(_.id) shouldEqual Some(checkpoint.id)
        state.stateContext.bytes shouldEqual checkpointContext.bytes
        state.fullBlockBodyFrontier shouldBe None
      }

      val recovered = DigestState.recover(
        idToVersion(checkpoint.id), checkpoint.stateRoot, checkpointContext, dir, settings).get
      try {
        checkCheckpoint(recovered)
      } finally {
        recovered.close()
      }

      val reopened = DigestState.create(None, None, dir, settings)
      try {
        checkCheckpoint(reopened)
      } finally {
        reopened.close()
      }
    } finally {
      us.closeStorage()
    }
  }

  private def withBodyFrontierChain(length: Int)
                                   (check: (DigestState, java.io.File, Seq[ErgoFullBlock], () => Unit) => Unit): Unit = {
    val isolatedSettings = settings.copy(directory = createTempDir.getAbsolutePath)
    var (us, bh) = createUtxoState(isolatedSettings)
    val dir = createTempDir
    val ds = DigestState.create(Some(us.version), Some(us.rootDigest), dir, settings)
    var closed = false
    def closeState(): Unit = if (!closed) {
      ds.close()
      closed = true
    }
    try {
      var parent: Option[ErgoFullBlock] = None
      val blocks = (1 to length).map { height =>
        val (block, nextBoxes) = validFullBlockWithBoxHolder(
          parent, us, bh, new RandomWrapper(Some(height)))
        us = us.applyModifier(block, None)(_ => ()).get
        bh = nextBoxes
        parent = Some(block)
        block
      }
      check(ds, dir, blocks, () => closeState())
    } finally {
      closeState()
      us.closeStorage()
    }
  }

  property("digest read bootstraps an empty directory without a frontier") {
    val directory = createTempDir
    directory.listFiles() shouldBe empty
    val fresh = DigestState.create(None, None, directory, settings)
    try {
      fresh.version shouldBe ErgoState.genesisStateVersion
      fresh.rootDigest shouldEqual settings.chainSettings.genesisStateDigest
      fresh.stateContext.lastHeaderOpt shouldBe None
      fresh.fullBlockBodyFrontier shouldBe None
    } finally fresh.close()
    val reopened = DigestState.create(None, None, directory, settings)
    try {
      reopened.version shouldBe ErgoState.genesisStateVersion
      reopened.fullBlockBodyFrontier shouldBe None
    } finally reopened.close()
  }

  private def checkCorruptPersistedRoot(replacement: Option[Array[Byte]]): Unit = {
    withBodyFrontierChain(1) { (state, directory, blocks, closeState) =>
      val applied = state.applyModifier(blocks.head, None)(_ => ()).get
      val versionKey = versionToBytes(applied.version)
      val contextBytes = applied.store.get(ErgoStateReader.ContextKey).get
      val certificateBytes = applied.store.get(DigestState.BodyFrontierKey).get
      val updates = replacement.toSeq.map(root => versionKey -> root)
      applied.store.update(versionKey, Seq(versionKey), updates).get
      val sentinel = new java.io.File(directory, "state-read-sentinel")
      val sentinelBytes = Array[Byte](31, 32, 33)
      Files.write(sentinel.toPath, sentinelBytes)
      closeState()

      (1 to 2).foreach { _ =>
        intercept[CriticalSystemException] {
          DigestState.create(None, None, directory, settings)
        }
      }
      Files.readAllBytes(sentinel.toPath) shouldEqual sentinelBytes
      val inspected = new LDBVersionedStore(directory, initialKeepVersions = settings.nodeSettings.keepVersions)
      try {
        inspected.lastVersionID.map(_.toSeq) shouldBe Some(versionKey.toSeq)
        inspected.get(versionKey).map(_.toSeq) shouldBe replacement.map(_.toSeq)
        inspected.get(DigestState.BodyFrontierKey).get shouldEqual certificateBytes
        inspected.get(ErgoStateReader.ContextKey).get shouldEqual contextBytes
      } finally inspected.close()
    }
  }

  property("digest read fails closed with a missing certified latest root") {
    checkCorruptPersistedRoot(None)
  }

  property("digest read fails closed with a short certified latest root") {
    checkCorruptPersistedRoot(Some(Array[Byte](1, 2, 3)))
  }

  property("digest read fails closed with a malformed present body frontier") {
    withBodyFrontierChain(1) { (state, directory, blocks, closeState) =>
      val applied = state.applyModifier(blocks.head, None)(_ => ()).get
      val versionKey = versionToBytes(applied.version)
      val contextBytes = applied.store.get(ErgoStateReader.ContextKey).get
      val malformedCertificate = Array[Byte](1)
      applied.store.update(versionKey, Seq.empty,
        Seq(DigestState.BodyFrontierKey -> malformedCertificate)).get
      val sentinel = new java.io.File(directory, "state-read-sentinel")
      val sentinelBytes = Array[Byte](41, 42, 43)
      Files.write(sentinel.toPath, sentinelBytes)
      closeState()

      (1 to 2).foreach { _ =>
        intercept[CriticalSystemException] {
          DigestState.create(None, None, directory, settings)
        }
      }
      Files.readAllBytes(sentinel.toPath) shouldEqual sentinelBytes
      val inspected = new LDBVersionedStore(directory, initialKeepVersions = settings.nodeSettings.keepVersions)
      try {
        inspected.lastVersionID.map(_.toSeq) shouldBe Some(versionKey.toSeq)
        inspected.get(versionKey).get shouldEqual applied.rootDigest
        inspected.get(DigestState.BodyFrontierKey).get shouldEqual malformedCertificate
        inspected.get(ErgoStateReader.ContextKey).get shouldEqual contextBytes
      } finally inspected.close()
    }
  }

  property("full body frontier persists across reopen and is immutable in an older reader") {
    withBodyFrontierChain(2) { (ds, dir, blocks, closeState) =>
      ds.fullBlockBodyFrontier shouldBe None
      val first = ds.applyModifier(blocks.head, None)(_ => ()).get
      first.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      ds.fullBlockBodyFrontier shouldBe None
      val second = first.applyModifier(blocks(1), None)(_ => ()).get
      second.getReader.fullBlockBodyFrontier shouldEqual Some(blocks(1).id)
      first.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      closeState()
      val reopened = DigestState.create(None, None, dir, settings)
      try {
        reopened.version shouldEqual blocks(1).id
        reopened.rootDigest shouldEqual blocks(1).header.stateRoot
        reopened.stateContext.lastHeaderOpt.map(_.id) shouldEqual Some(blocks(1).id)
        reopened.fullBlockBodyFrontier shouldEqual Some(blocks(1).id)
      } finally {
        reopened.close()
      }
    }
  }

  property("header-only prefix and a later full body do not manufacture a frontier") {
    withBodyFrontierChain(2) { (ds, dir, blocks, closeState) =>
      val headerOnly = ds.applyModifier(blocks.head.header, None)(_ => ()).get
      headerOnly.fullBlockBodyFrontier shouldBe None
      val laterBody = headerOnly.applyModifier(blocks(1), None)(_ => ()).get
      laterBody.fullBlockBodyFrontier shouldBe None
      closeState()
      val reopened = DigestState.create(None, None, dir, settings)
      try {
        reopened.version shouldEqual blocks(1).id
        reopened.fullBlockBodyFrontier shouldBe None
      } finally {
        reopened.close()
      }
    }
  }

  property("header-only gap cannot be leapfrogged by full body B13") {
    withBodyFrontierChain(13) { (ds, _, blocks, _) =>
      val first = ds.applyModifier(blocks.head, None)(_ => ()).get
      val headerOnly = blocks.slice(1, 12).foldLeft(first) { (state, block) =>
        val next = state.applyModifier(block.header, None)(_ => ()).get
        next.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
        next
      }
      val laterBody = headerOnly.applyModifier(blocks(12), None)(_ => ()).get
      laterBody.version shouldEqual blocks(12).id
      laterBody.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
    }
  }

  property("frontier is cleared by a synthetic genesis reset and remains absent after reopen") {
    withBodyFrontierChain(1) { (ds, dir, blocks, closeState) =>
      ds.applyModifier(blocks.head, None)(_ => ()).get.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      closeState()
      val reset = DigestState.create(
        Some(ErgoState.genesisStateVersion), Some(settings.chainSettings.genesisStateDigest), dir, settings)
      try {
        reset.version shouldEqual ErgoState.genesisStateVersion
        reset.fullBlockBodyFrontier shouldBe None
      } finally {
        reset.close()
      }
      val reopened = DigestState.create(None, None, dir, settings)
      try {
        reopened.version shouldEqual ErgoState.genesisStateVersion
        reopened.fullBlockBodyFrontier shouldBe None
      } finally {
        reopened.close()
      }
    }
  }

  property("frontier does not advance for a checkpoint body referencing the pre-gap parent") {
    withBodyFrontierChain(3) { (ds, dir, blocks, closeState) =>
      val first = ds.applyModifier(blocks.head, None)(_ => ()).get
      val headerOnly = first.applyModifier(blocks(1).header, None)(_ => ()).get
      val next = blocks(2).copy(header = blocks(2).header.copy(parentId = blocks.head.id))
      headerOnly.validate(next) shouldBe 'failure
      closeState()
      val checkpointSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
        checkpoint = Some(CheckpointSettings(next.header.height, next.id))))
      val reopened = DigestState.create(None, None, dir, checkpointSettings)
      try {
        reopened.validate(next) shouldBe 'success
        val applied = reopened.applyModifier(next, None)(_ => ()).get
        applied.version shouldEqual next.id
        applied.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      } finally {
        reopened.close()
      }
    }
  }

  property("frontier requires the current state version to match the body parent") {
    withBodyFrontierChain(2) { (ds, dir, blocks, closeState) =>
      val first = ds.applyModifier(blocks.head, None)(_ => ()).get
      val otherVersion = bytesToVersion(Array.fill(32)(42: Byte))
      val versionBytes = org.ergoplatform.core.versionToBytes(otherVersion)
      first.store.update(versionBytes, Seq.empty, Seq(versionBytes -> first.rootDigest)).get
      closeState()
      val reopened = DigestState.create(None, None, dir, settings)
      try {
        reopened.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
        reopened.stateContext.lastHeaderOpt.map(_.id) shouldEqual Some(blocks.head.id)
        reopened.validate(blocks(1)) shouldBe 'success
        reopened.applyModifier(blocks(1), None)(_ => ()).get.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      } finally {
        reopened.close()
      }
    }
  }

  property("frontier requires the current context tip to match the body parent") {
    withBodyFrontierChain(3) { (ds, dir, blocks, closeState) =>
      val first = ds.applyModifier(blocks.head, None)(_ => ()).get
      val context = first.stateContext.appendHeader(blocks(1).header).get
      val versionBytes = org.ergoplatform.core.versionToBytes(first.version)
      first.store.update(versionBytes, Seq.empty, Seq(
        versionBytes -> blocks(1).header.stateRoot, ErgoStateReader.ContextKey -> context.bytes)).get
      closeState()
      val next = blocks(2).copy(header = blocks(2).header.copy(parentId = blocks.head.id))
      val checkpointSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
        checkpoint = Some(CheckpointSettings(next.header.height, next.id))))
      val reopened = DigestState.create(None, None, dir, checkpointSettings)
      try {
        reopened.version shouldEqual first.version
        reopened.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
        reopened.validate(next) shouldBe 'success
        reopened.applyModifier(next, None)(_ => ()).get.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      } finally {
        reopened.close()
      }
    }
  }

  property("frontier requires the current root to match the certified parent context") {
    withBodyFrontierChain(2) { (ds, dir, blocks, closeState) =>
      val first = ds.applyModifier(blocks.head, None)(_ => ()).get
      ds.rootDigest.sameElements(first.rootDigest) shouldBe false
      val versionBytes = org.ergoplatform.core.versionToBytes(first.version)
      first.store.update(versionBytes, Seq.empty, Seq(versionBytes -> ds.rootDigest)).get
      closeState()
      val next = blocks(1)
      val checkpointSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
        checkpoint = Some(CheckpointSettings(next.header.height, next.id))))
      val reopened = DigestState.create(None, None, dir, checkpointSettings)
      try {
        reopened.version shouldEqual first.version
        reopened.stateContext.lastHeaderOpt.map(_.id) shouldEqual Some(blocks.head.id)
        reopened.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
        reopened.rootDigest.sameElements(blocks.head.header.stateRoot) shouldBe false
        reopened.validate(next) shouldBe 'success
        reopened.applyModifier(next, None)(_ => ()).get.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      } finally reopened.close()
    }
  }

  property("frontier bootstrap requires the actual genesis state version") {
    withBodyFrontierChain(1) { (ds, _, blocks, _) =>
      val dir = createTempDir
      val otherVersion = bytesToVersion(Array.fill(32)(42: Byte))
      val synthetic = DigestState.create(
        Some(otherVersion), Some(settings.chainSettings.genesisStateDigest), dir, settings)
      try {
        synthetic.validate(blocks.head) shouldBe 'success
        synthetic.applyModifier(blocks.head, None)(_ => ()).get.fullBlockBodyFrontier shouldBe None
      } finally {
        synthetic.close()
      }
      ds.fullBlockBodyFrontier shouldBe None
    }
  }

  property("rollback restores the full body frontier and rejects an unavailable version") {
    withBodyFrontierChain(2) { (ds, _, blocks, _) =>
      val first = ds.applyModifier(blocks.head, None)(_ => ()).get
      val second = first.applyModifier(blocks(1), None)(_ => ()).get
      val unavailable = bytesToVersion(Array.fill(32)(42: Byte))
      val failed = second.rollbackTo(unavailable)
      failed shouldBe 'failure
      failed.failed.get.getMessage shouldEqual "versionID not found, can not rollback"
      second.store.lastVersionID.get shouldEqual org.ergoplatform.core.versionToBytes(second.version)
      val restored = second.rollbackTo(first.version).get
      restored.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      restored.rootDigest shouldEqual first.rootDigest
      restored.stateContext.lastHeaderOpt.map(_.id) shouldEqual Some(blocks.head.id)
      restored.rollbackTo(ds.version).get.fullBlockBodyFrontier shouldBe None
    }
  }

  property("checkpoint recovery clears an existing full body frontier") {
    withBodyFrontierChain(1) { (ds, dir, blocks, closeState) =>
      val applied = ds.applyModifier(blocks.head, None)(_ => ()).get
      applied.fullBlockBodyFrontier shouldEqual Some(blocks.head.id)
      val context = applied.stateContext
      closeState()
      val recovered = DigestState.recover(
        applied.version, applied.rootDigest, context, dir, settings).get
      try {
        recovered.version shouldEqual applied.version
        recovered.fullBlockBodyFrontier shouldBe None
      } finally {
        recovered.close()
      }
      val reopened = DigestState.create(None, None, dir, settings)
      try {
        reopened.version shouldEqual applied.version
        reopened.fullBlockBodyFrontier shouldBe None
      } finally {
        reopened.close()
      }
    }
  }

  property("reopen") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh, parameters)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val fb = validFullBlock(parentOpt = None, us, bh)
      val dir2 = createTempDir
      val ds = DigestState.create(Some(us.version), Some(us.rootDigest), dir2, settings)
      ds.applyModifier(fb, None)(_ => ()) shouldBe 'success
      ds.close()

      val state = DigestState.create(None, None, dir2, settings)
      state.version shouldEqual fb.header.id
      state.rootDigest shouldEqual fb.header.stateRoot
      state.close()
      us.closeStorage()
    }
  }

  property("validate() - valid block") {
    var (us, bh) = createUtxoState(settings)
    var ds = createDigestState(us.version, us.rootDigest)
    var parentOpt: Option[ErgoFullBlock] = None

    forAll { seed: Int =>
      val blBh = validFullBlockWithBoxHolder(parentOpt, us, bh, new RandomWrapper(Some(seed)))
      val block = blBh._1
      bh = blBh._2
      ds = ds.applyModifier(block, None)(_ => ()).get
      us = us.applyModifier(block, None)(_ => ()).get
      parentOpt = Some(block)
    }
  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(emptyVersion, emptyAdDigest)
      state.validate(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh, parameters)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)
      block.blockTransactions.transactions.exists(_.dataInputs.nonEmpty) shouldBe true

      val ds = createDigestState(us.version, us.rootDigest)
      ds.applyModifier(block, None)(_ => ()) shouldBe 'success
      ds.close()
      us.closeStorage()
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(emptyVersion, emptyAdDigest)
      state.applyModifier(b, None)(_ => ()).isFailure shouldBe true
    }
  }

  property("rollback & rollback versions") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh, parameters)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.version, us.rootDigest)

      ds.rollbackVersions.size shouldEqual 1

      val ds2 = ds.applyModifier(block, None)(_ => ()).get

      ds2.rollbackVersions.size shouldEqual 2

      ds2.stateContext.lastHeaders.size shouldEqual 1

      java.util.Arrays.equals(ds2.rootDigest, ds.rootDigest) shouldBe false

      val ds3 = ds2.rollbackTo(ds.version).get
      ds3.rootDigest shouldBe ds.rootDigest

      ds3.stateContext.lastHeaders.size shouldEqual 0

      ds3.applyModifier(block, None)(_ => ()).get.rootDigest shouldBe ds2.rootDigest

      ds.close()
      us.closeStorage()
    }
  }

  property("validateTransactions() - dataInputs") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh, parameters)
      val ds = createDigestState(us.version, us.rootDigest)

      // generate 2 independent transactions spending state boxes only
      val headTx = validTransactionsFromBoxes(1, bh.boxes.take(10).values.toSeq, new RandomWrapper())._1.head
      val nextTx = validTransactionsFromBoxes(1, bh.boxes.takeRight(10).values.toSeq, new RandomWrapper())._1.head
      headTx.inputs.intersect(nextTx.inputs) shouldBe empty

      // trying to apply transactions with data inputs same as inputs of the next tx
      val dataInputs = nextTx.inputs.filter(i => us.boxById(i.boxId).isDefined).map(i => DataInput(i.boxId))
      val inputs = headTx.outputs.map(b =>  Input(b.id, ProverResult.empty))
      val txWithDataInputs = ErgoTransaction(inputs, dataInputs, headTx.outputCandidates)

      val txs1 = IndexedSeq(headTx, nextTx, txWithDataInputs)
      val (proofBytes1, digest1) = us.proofsForTransactions(txs1).get
      val proof1 = ADProofs(defaultHeaderGen.sample.get.id, proofBytes1)
      ds.validateTransactions(txs1, digest1, proof1, emptyStateContext) shouldBe 'success

      val txs2 = IndexedSeq(headTx, txWithDataInputs, nextTx)
      val (proofBytes2, digest2) = us.proofsForTransactions(txs2).get
      val proof2 = ADProofs(defaultHeaderGen.sample.get.id, proofBytes2)
      ds.validateTransactions(txs2, digest2, proof2, emptyStateContext) shouldBe 'success

      val txs3 = IndexedSeq(txWithDataInputs, headTx, nextTx)
      ds.validateTransactions(txs3, digest2, proof2, emptyStateContext) shouldBe 'failure

      ds.close()
      us.closeStorage()
    }
  }

}
