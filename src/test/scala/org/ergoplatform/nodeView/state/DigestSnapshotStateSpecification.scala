package org.ergoplatform.nodeView.state

import java.io.File
import java.nio.file.Files
import org.ergoplatform.core.{VersionTag, idToVersion, versionToBytes}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
import org.ergoplatform.wallet.utils.FileUtils
import scorex.crypto.authds.ADDigest
import scorex.db.LDBVersionedStore

import scala.util.{Failure, Try}

/** Direct checkpoint contracts; snapshot reconstruction and actor publication have separate fixtures. */
class DigestSnapshotStateSpecification extends ErgoCorePropertyTest with FileUtils {

  private def contextFor(header: Header): ErgoStateContext = {
    val empty = ErgoStateContext.empty(settings.chainSettings, settings.launchParameters)
    new ErgoStateContext(Seq(header), None, empty.genesisStateDigest, empty.currentParameters,
      empty.validationSettings, empty.votingData)(settings.chainSettings)
  }

  private def changed(bytes: Array[Byte]): Array[Byte] = {
    val result = bytes.clone()
    result(0) = (result(0) ^ 1).toByte
    result
  }

  private class SnapshotStore(dir: File) extends LDBVersionedStore(dir, initialKeepVersions = 10) {
    val writeError = new IllegalStateException("snapshot checkpoint write failed")
    val cleanError = new IllegalStateException("snapshot checkpoint cleanup failed")
    var failWrite = false
    var failWriteOnCall: Option[Int] = None
    var writeCalls = 0
    var failClean = false
    var cleanCalls = 0
    private var closed = false

    override def close(): Unit = if (!closed) {
      super.close()
      closed = true
    }

    override def update(versionID: Array[Byte],
                        toRemove: TraversableOnce[Array[Byte]],
                        toUpdate: TraversableOnce[(Array[Byte], Array[Byte])]): Try[Unit] = {
      writeCalls += 1
      if (failWrite || failWriteOnCall.contains(writeCalls)) Failure(writeError)
      else super.update(versionID, toRemove, toUpdate)
    }

    override def clean(count: Int): Unit = {
      cleanCalls += 1
      if (failClean) throw cleanError
      super.clean(count)
    }
  }

  private def withStore(test: (SnapshotStore, File, ErgoSettings) => Unit): Unit = {
    val root = Files.createTempDirectory("digest-snapshot-checkpoint-").toFile
    val dir = new File(root, "state")
    Files.createDirectories(dir.toPath)
    val localSettings = settings.copy(directory = root.getAbsolutePath,
      nodeSettings = settings.nodeSettings.copy(stateType = StateType.Digest, keepVersions = 10))
    val store = new SnapshotStore(dir)
    try test(store, dir, localSettings)
    finally {
      store.close()
      deleteRecursive(root)
    }
  }

  private def seed(store: SnapshotStore, root: Array[Byte]): Unit = {
    // Model the two version namespaces at this helper's input, without claiming AVL reconstruction.
    store.update(Array.fill[Byte](32)(7), Seq.empty, Seq(Array[Byte](1) -> Array[Byte](2))).get
    store.update(root, Seq.empty, Seq(Array[Byte](3) -> Array[Byte](4))).get
  }

  Seq("legacy AVL", "Digest checkpoint").foreach { format =>
    property(s"read snapshot preserves $format identity and a durable rollback anchor") {
      val header = defaultHeaderGen.sample.get
      val version = idToVersion(header.id)
      val context = contextFor(header)
      withStore { (store, dir, localSettings) =>
        seed(store, header.stateRoot)
        val initialVersion = if (format == "legacy AVL") header.stateRoot else versionToBytes(version)
        store.update(initialVersion, Seq.empty,
          UtxoState.metadata(version, header.stateRoot, None, context)).get
        store.close()

        // Repeated verified reopening must retain the checkpoint's undo anchor.
        (1 to 2).foreach { _ =>
          val state = DigestState.readSnapshot(dir, localSettings, version, header.stateRoot, context).get
          try {
            state.version shouldBe version
            state.rootDigest.toSeq shouldBe header.stateRoot.toSeq
            state.stateContext.bytes.toSeq shouldBe context.bytes.toSeq
            state.rollbackVersions.toSeq shouldBe Seq(version)
            state.store.lastVersionID.get.toSeq shouldBe versionToBytes(version).toSeq
          } finally state.close()
        }

        val successor = header.copy(timestamp = header.timestamp + 1, stateRoot = ADDigest @@ changed(header.stateRoot))
        val successorVersion = idToVersion(successor.id)
        val state = DigestState.readSnapshot(dir, localSettings, version, header.stateRoot, context).get
        try {
          // Direct store successor isolates undo retention; actor/full-block validation is tested separately.
          state.store.update(versionToBytes(successorVersion), Seq.empty, Seq(
            versionToBytes(successorVersion) -> successor.stateRoot,
            ErgoStateReader.ContextKey -> contextFor(successor).bytes)).get
        } finally state.close()

        val reopened = DigestState.create(None, None, dir, localSettings)
        try {
          reopened.version shouldBe successorVersion
          reopened.rollbackVersions.toSeq shouldBe Seq(successorVersion, version)
          val rolledBack = reopened.rollbackTo(version).get
          rolledBack.rootDigest.toSeq shouldBe header.stateRoot.toSeq
          rolledBack.stateContext.bytes.toSeq shouldBe context.bytes.toSeq
        } finally reopened.close()
      }
    }
  }

  Seq("wrong root", "wrong raw context", "wrong version", "missing root", "missing context").foreach { fault =>
    property(s"read snapshot rejects $fault without changing stored evidence") {
      val header = defaultHeaderGen.sample.get
      val version = idToVersion(header.id)
      val context = contextFor(header)
      val versionBytes = versionToBytes(version)
      withStore { (store, dir, localSettings) =>
        seed(store, header.stateRoot)
        val rawRoot = if (fault == "wrong root") changed(header.stateRoot) else header.stateRoot
        val rawContext = if (fault == "wrong raw context") Array[Byte](0) else context.bytes
        val storeVersion = if (fault == "wrong version") changed(versionBytes) else header.stateRoot
        val rows = Seq(
          versionBytes -> rawRoot,
          ErgoStateReader.ContextKey -> rawContext).filterNot { case (key, _) =>
          (fault == "missing root" && key.sameElements(versionBytes)) ||
            (fault == "missing context" && key.sameElements(ErgoStateReader.ContextKey))
        }
        store.update(storeVersion, Seq.empty, rows).get
        val keys = Seq(versionBytes, ErgoStateReader.ContextKey, Array[Byte](1), Array[Byte](3),
          versionToBytes(ErgoState.genesisStateVersion))
        val valuesBefore = keys.map(key => store.get(key).map(_.toSeq))
        val versionsBefore = store.rollbackVersions().map(_.toSeq).toSeq
        val lastBefore = store.lastVersionID.map(_.toSeq)
        store.close()

        // Missing/corrupt metadata must not enter DigestState.create's genesis fallback.
        DigestState.readSnapshot(dir, localSettings, version, header.stateRoot, context).isFailure shouldBe true
        val inspected = new LDBVersionedStore(dir, initialKeepVersions = 10)
        try {
          keys.map(key => inspected.get(key).map(_.toSeq)) shouldBe valuesBefore
          inspected.rollbackVersions().map(_.toSeq).toSeq shouldBe versionsBefore
          inspected.lastVersionID.map(_.toSeq) shouldBe lastBefore
        } finally inspected.close()
      }
    }
  }

  property("snapshot checkpoint persists header identity and context across store reopen") {
    val header = defaultHeaderGen.sample.get
    val context = contextFor(header)
    withStore { (store, dir, localSettings) =>
      header.stateRoot.length shouldBe 33
      seed(store, header.stateRoot)
      val state = DigestState.fromSnapshot(idToVersion(header.id), header.stateRoot,
        context, store, localSettings).get
      state.store should be theSameInstanceAs store
      state.version shouldBe idToVersion(header.id)
      state.rootDigest.toSeq shouldBe header.stateRoot.toSeq
      state.stateContext.bytes.toSeq shouldBe context.bytes.toSeq
      store.lastVersionID.get.toSeq shouldBe versionToBytes(state.version).toSeq
      state.rollbackVersions.toSeq shouldBe Seq(idToVersion(header.id))
      store.close()

      val reopened = DigestState.create(None, None, dir, localSettings)
      try {
        reopened.version shouldBe idToVersion(header.id)
        reopened.rootDigest.toSeq shouldBe header.stateRoot.toSeq
        reopened.stateContext.bytes.toSeq shouldBe context.bytes.toSeq
        reopened.rollbackVersions.toSeq shouldBe Seq(idToVersion(header.id))
        reopened.rollbackTo(idToVersion(header.id)).get.rootDigest.toSeq shouldBe header.stateRoot.toSeq
      } finally reopened.close()
    }
  }

  property("fresh snapshot checkpoint retains rollback anchor before any verified reopen") {
    val header = defaultHeaderGen.sample.get
    val version = idToVersion(header.id)
    val context = contextFor(header)
    val successor = header.copy(timestamp = header.timestamp + 1, stateRoot = ADDigest @@ changed(header.stateRoot))
    val successorVersion = idToVersion(successor.id)
    withStore { (store, dir, localSettings) =>
      seed(store, header.stateRoot)
      val state = DigestState.fromSnapshot(version, header.stateRoot, context, store, localSettings).get
      // Simulate the Digest metadata consumer before the first restart, without readSnapshot anchor repair.
      state.store.update(versionToBytes(successorVersion), Seq.empty, Seq(
        versionToBytes(successorVersion) -> successor.stateRoot,
        ErgoStateReader.ContextKey -> contextFor(successor).bytes)).get
      store.close()

      val reopened = DigestState.create(None, None, dir, localSettings)
      try {
        reopened.version shouldBe successorVersion
        reopened.rollbackVersions.toSeq shouldBe Seq(successorVersion, version)
        reopened.store.rollbackVersions().forall(_.length == 32) shouldBe true
        val rolledBack = reopened.rollbackTo(version).get
        rolledBack.version shouldBe version
        rolledBack.rootDigest.toSeq shouldBe header.stateRoot.toSeq
        rolledBack.stateContext.bytes.toSeq shouldBe context.bytes.toSeq
      } finally reopened.close()
    }
  }

  property("snapshot checkpoint propagates the write failure without cleaning") {
    val header = defaultHeaderGen.sample.get
    withStore { (store, _, localSettings) =>
      seed(store, header.stateRoot)
      store.failWrite = true
      val result = DigestState.fromSnapshot(idToVersion(header.id), header.stateRoot,
        contextFor(header), store, localSettings)
      result.failed.get should be theSameInstanceAs store.writeError
      store.cleanCalls shouldBe 0
      store.lastVersionID.get.toSeq shouldBe header.stateRoot.toSeq
      store.get(versionToBytes(idToVersion(header.id))) shouldBe None
    }
  }

  property("snapshot checkpoint propagates failure to retain its undo anchor") {
    val header = defaultHeaderGen.sample.get
    withStore { (store, _, localSettings) =>
      seed(store, header.stateRoot)
      store.failWriteOnCall = Some(store.writeCalls + 2)
      val result = DigestState.fromSnapshot(idToVersion(header.id), header.stateRoot,
        contextFor(header), store, localSettings)
      result.failed.get should be theSameInstanceAs store.writeError
      store.cleanCalls shouldBe 1
      store.lastVersionID.get.toSeq shouldBe versionToBytes(idToVersion(header.id)).toSeq
    }
  }

  property("snapshot checkpoint propagates cleanup failure after metadata persistence") {
    val header = defaultHeaderGen.sample.get
    withStore { (store, _, localSettings) =>
      seed(store, header.stateRoot)
      store.failClean = true
      val result = DigestState.fromSnapshot(idToVersion(header.id), header.stateRoot,
        contextFor(header), store, localSettings)
      result.failed.get should be theSameInstanceAs store.cleanError
      store.cleanCalls shouldBe 1
      store.lastVersionID.get.toSeq shouldBe versionToBytes(idToVersion(header.id)).toSeq
      store.get(versionToBytes(idToVersion(header.id))).get.toSeq shouldBe header.stateRoot.toSeq
    }
  }

  Seq("version", "context root", "context header", "empty context", "store version").foreach { fault =>
    property(s"snapshot checkpoint rejects isolated $fault mismatch before writing") {
      val header = defaultHeaderGen.sample.get
      val wrongRoot = ADDigest @@ changed(header.stateRoot)
      val fork = header.copy(timestamp = header.timestamp + 1)
      val version = if (fault == "version") idToVersion(fork.id) else idToVersion(header.id)
      val root = if (fault == "context root") wrongRoot else header.stateRoot
      val context = fault match {
        case "context header" => contextFor(fork)
        case "empty context" => ErgoStateContext.empty(settings.chainSettings, settings.launchParameters)
        case _ => contextFor(header)
      }
      withStore { (store, _, localSettings) =>
        val storeVersion = if (fault == "store version") changed(root) else root
        seed(store, storeVersion)
        val result = DigestState.fromSnapshot(version, root, context, store, localSettings)
        result.isFailure shouldBe true
        store.lastVersionID.get.toSeq shouldBe storeVersion.toSeq
        store.get(versionToBytes(version)) shouldBe None
        store.cleanCalls shouldBe 0
      }
    }
  }

  property("prepared Digest snapshot requires each identity and context signal") {
    val header = defaultHeaderGen.sample.get
    val context = contextFor(header).bytes
    val fork = header.copy(timestamp = header.timestamp + 1)
    fork.stateRoot.toSeq shouldBe header.stateRoot.toSeq
    fork.id should not be header.id

    case class Signals(kind: Boolean = true, bootstrap: Boolean = true, applied: Boolean = true,
                       version: VersionTag = idToVersion(header.id), root: Array[Byte] = header.stateRoot,
                       canonical: Option[Header] = Some(header), stored: Option[Array[Byte]] = Some(context),
                       expected: Option[Array[Byte]] = Some(context))
    def accepts(s: Signals): Boolean = ErgoNodeViewHolder.isPreparedDigestSnapshotState(
      s.kind, s.bootstrap, s.applied, s.version, s.root, s.canonical, s.stored, s.expected)

    accepts(Signals()) shouldBe true
    val cases = Seq(
      "state kind" -> Signals(kind = false),
      "bootstrap disabled" -> Signals(bootstrap = false),
      "applied marker missing" -> Signals(applied = false),
      "wrong version" -> Signals(version = idToVersion(fork.id)),
      "wrong root" -> Signals(root = changed(header.stateRoot)),
      "canonical header missing" -> Signals(canonical = None),
      "same-root canonical sibling" -> Signals(canonical = Some(fork)),
      "stored context missing" -> Signals(stored = None),
      "stored context different" -> Signals(stored = Some(changed(context))),
      "expected context missing" -> Signals(expected = None),
      "expected context different" -> Signals(expected = Some(changed(context)))
    )
    cases.foreach { case (clue, signals) => withClue(clue) { accepts(signals) shouldBe false } }
  }

  property("prepared Digest snapshot stops reading after the first failed trust signal") {
    val header = defaultHeaderGen.sample.get
    val context = contextFor(header).bytes
    (0 to 4).foreach { stop =>
      var reads = Vector.empty[Int]
      def gate(index: Int): Boolean = {
        reads :+= index
        index != stop
      }
      val accepted = ErgoNodeViewHolder.isPreparedDigestSnapshotState(
        stateIsDigest = stop != 0,
        utxoBootstrap = gate(1),
        snapshotApplied = gate(2),
        stateVersion = idToVersion(header.id),
        stateRoot = header.stateRoot,
        snapshotHeaderOpt = if (gate(3)) Some(header) else None,
        storedContext = if (gate(4)) Some(context) else None,
        expectedContext = { reads :+= 5; Some(context) })
      accepted shouldBe false
      reads shouldBe (1 to stop).toVector
    }
  }
}
