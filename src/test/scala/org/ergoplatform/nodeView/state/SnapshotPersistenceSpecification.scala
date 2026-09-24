package org.ergoplatform.nodeView.state

import com.google.common.primitives.Ints
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import java.util.concurrent.{CountDownLatch, TimeUnit}
import org.ergoplatform.serialization.{ManifestSerializer, SubtreeSerializer}
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.Insert
import scorex.crypto.authds.avltree.batch.helpers.TestHelper
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.encode.Base16

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class SnapshotPersistenceSpecification extends AnyFunSuite with Matchers with TestHelper {
  override protected val KL: Int = 32
  override protected val VL: Int = 8

  private val metadataKey = Array.fill(32)(0: Byte)

  private class Destination extends LDBKVStore(null) {
    val contents: mutable.Map[String, Array[Byte]] = mutable.Map.empty
    val writes: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty
    var failAt: Int = Int.MaxValue
    val writeError = new IllegalStateException("destination write unavailable")
    var removals = 0

    override def insert(key: K, value: V): Try[Unit] = {
      writes += Base16.encode(key)
      if (writes.size == failAt) Failure(writeError)
      else {
        contents(Base16.encode(key)) = value.clone()
        Success(())
      }
    }

    override def get(key: K): Option[V] = contents.get(Base16.encode(key)).map(_.clone())

    override def remove(keys: Array[K]): Try[Unit] = {
      removals += 1
      keys.foreach(key => contents.remove(Base16.encode(key)))
      Success(())
    }
  }

  private def key(i: Int): ADKey = ADKey @@ (Array.fill(28)(0: Byte) ++ Ints.toByteArray(i))

  private def withSource[A](count: Int)(f: PERSISTENT_PROVER => A): A = {
    val store = createVersionedStore()
    try {
      val prover = createPersistentProver(createVersionedStorage(store))
      (1 to count).foreach(i => prover.performOneOperation(Insert(key(i), ADValue @@ Array.fill(8)(1: Byte))).get)
      prover.generateProofAndUpdateStorage()
      f(prover)
    } finally store.close()
  }

  Seq("left subtree", "right subtree", "manifest", "availability metadata").zipWithIndex.foreach {
    case (stage, index) =>
      test(s"snapshot returns the $stage write failure and stops publication") {
        withSource(8) { prover =>
          val destination = new Destination
          destination.failAt = index + 1
          val snapshots = new SnapshotsDb(destination)
          val result = snapshots.writeSnapshot(prover.storage.asInstanceOf[STORAGE], 10,
            prover.digest.dropRight(1), manifestDepth = 1)

          result shouldBe Failure(destination.writeError)
          destination.writes.size shouldBe index + 1
          snapshots.readSnapshotsInfo.availableManifests shouldBe empty
          if (index < 3) destination.get(prover.digest.dropRight(1)) shouldBe None
        }
      }
  }

  test("snapshot publishes metadata only after readable manifest and subtree content") {
    withSource(8) { prover =>
      val destination = new Destination
      val snapshots = new SnapshotsDb(destination)
      val root = snapshots.writeSnapshot(prover.storage.asInstanceOf[STORAGE], 10,
        prover.digest.dropRight(1), manifestDepth = 1).get
      val manifest = new ManifestSerializer(1).parseBytes(destination.get(root).get)
      manifest.subtreesIds.size shouldBe 2
      manifest.subtreesIds.foreach { id =>
        SubtreeSerializer.parseBytes(destination.get(id).get).id.sameElements(id) shouldBe true
      }
      destination.writes.last shouldBe Base16.encode(metadataKey)
      snapshots.readSnapshotsInfo.availableManifests(10).sameElements(root) shouldBe true
    }
  }

  test("changed source root does not write destination content or metadata") {
    withSource(8) { prover =>
      val destination = new Destination
      new SnapshotsDb(destination).writeSnapshot(prover.storage.asInstanceOf[STORAGE], 10,
        Array.fill(32)(7: Byte), manifestDepth = 1).isFailure shouldBe true
      destination.writes shouldBe empty
    }
  }

  private class AsyncPersistence(settings: ErgoSettings, prover: PERSISTENT_PROVER, database: SnapshotsDb)
    extends UtxoSetSnapshotPersistence {
    override protected def ergoSettings: ErgoSettings = settings
    override protected def persistentProver: PERSISTENT_PROVER = prover
    override private[nodeView] val snapshotsDb: SnapshotsDb = database
    def start(): Unit = saveSnapshotIfNeeded(9, Some(9))
  }

  Seq(1, 2, Int.MaxValue).foreach { failAt =>
    val outcome = if (failAt == Int.MaxValue) "success" else s"write failure $failAt"
    test(s"asynchronous persistence observes $outcome before pruning") {
      withSource(8) { prover =>
        val destination = new Destination
        destination.failAt = failAt
        var pruneCalls = 0
        var publishedBeforePrune = false
        val database = new SnapshotsDb(destination) {
          override def pruneSnapshots(toStore: Int): Unit = {
            pruneCalls += 1
            publishedBeforePrune = readSnapshotsInfo.availableManifests.contains(9)
            super.pruneSnapshots(toStore)
          }
        }
        val defaults = ErgoSettingsReader.read()
        val settings = defaults.copy(directory = getRandomTempDir.getAbsolutePath,
          chainSettings = defaults.chainSettings.copy(makeSnapshotEvery = 10),
          nodeSettings = defaults.nodeSettings.copy(utxoSettings =
            defaults.nodeSettings.utxoSettings.copy(storingUtxoSnapshots = 1)))
        val persistence = new AsyncPersistence(settings, prover, database)
        val logger = LoggerFactory.getLogger(classOf[AsyncPersistence]).asInstanceOf[Logger]
        val completed = new CountDownLatch(1)
        var terminalEvent: ILoggingEvent = null
        val appender = new AppenderBase[ILoggingEvent] {
          override def append(event: ILoggingEvent): Unit = {
            if (event.getFormattedMessage.startsWith("Work within future finished") ||
                event.getFormattedMessage.startsWith("Snapshot persistence failed")) {
              terminalEvent = event
              completed.countDown()
            }
          }
        }
        val previousLevel = logger.getLevel
        logger.setLevel(Level.INFO)
        appender.start()
        logger.addAppender(appender)
        try {
          persistence.start()
          completed.await(10, TimeUnit.SECONDS) shouldBe true
          if (failAt == Int.MaxValue) {
            pruneCalls shouldBe 1
            publishedBeforePrune shouldBe true
            terminalEvent.getLevel shouldBe Level.INFO
          } else {
            pruneCalls shouldBe 0
            database.readSnapshotsInfo.availableManifests shouldBe empty
            terminalEvent.getLevel shouldBe Level.ERROR
            terminalEvent.getThrowableProxy.getMessage shouldBe destination.writeError.getMessage
          }
        } finally {
          logger.detachAppender(appender)
          logger.setLevel(previousLevel)
          appender.stop()
          LDBFactory.createKvDb(s"${settings.directory}/snapshots").close()
        }
      }
    }
  }

  private case class RetentionFixture(contents: Map[String, Array[Byte]], roots: Vector[Array[Byte]])

  // Ordinary producer-generated trees, shared across cases as immutable serialized bytes.
  private lazy val retentionFixture: RetentionFixture = withSource(32768) { prover =>
    val destination = new Destination
    val snapshots = new SnapshotsDb(destination)
    def save(height: Int): Array[Byte] =
      snapshots.writeSnapshot(prover.storage.asInstanceOf[STORAGE], height, prover.digest.dropRight(1)).get
    val first = save(10)
    prover.performOneOperation(scorex.crypto.authds.avltree.batch.Update(key(1), ADValue @@ Array.fill(8)(2: Byte))).get
    prover.generateProofAndUpdateStorage()
    val second = save(20)
    prover.performOneOperation(scorex.crypto.authds.avltree.batch.Update(key(32768), ADValue @@ Array.fill(8)(2: Byte))).get
    prover.generateProofAndUpdateStorage()
    val third = save(30)
    RetentionFixture(destination.contents.toMap, Vector(first, second, third))
  }

  private def seededRetention(): (Destination, SnapshotsDb) = {
    val destination = new Destination
    destination.contents ++= retentionFixture.contents
    destination -> new SnapshotsDb(destination)
  }

  private def subtreeIds(root: Array[Byte]): Set[String] =
    ManifestSerializer.defaultSerializer.parseBytes(retentionFixture.contents(Base16.encode(root)))
      .subtreesIds.map(Base16.encode).toSet

  test("pruning protects fragments referenced by any retained manifest") {
    val (destination, snapshots) = seededRetention()
    val Vector(first, second, third) = retentionFixture.roots
    val sharedWithOlderRetainedOnly = (subtreeIds(first) intersect subtreeIds(second)) -- subtreeIds(third)
    sharedWithOlderRetainedOnly should not be empty
    val obsolete = subtreeIds(first) -- subtreeIds(second) -- subtreeIds(third)
    obsolete should not be empty

    snapshots.pruneSnapshots(2)

    snapshots.readSnapshotsInfo.availableManifests.keySet shouldBe Set(20, 30)
    destination.get(first) shouldBe None
    obsolete.foreach(id => destination.contents.contains(id) shouldBe false)
    Seq(second, third).foreach { root =>
      destination.get(root).isDefined shouldBe true
      subtreeIds(root).foreach { id =>
        val bytes = destination.contents.getOrElse(id, fail(s"Retained fragment missing: $id"))
        Base16.encode(SubtreeSerializer.parseBytes(bytes).id) shouldBe id
      }
    }
  }

  Seq(1, 2).foreach { retainedIndex =>
    test(s"missing retained manifest $retainedIndex prevents pruning and metadata changes") {
      val (destination, snapshots) = seededRetention()
      destination.contents.remove(Base16.encode(retentionFixture.roots(retainedIndex)))
      val before = destination.contents.keySet.toSet
      val metadata = destination.get(metadataKey).get

      snapshots.pruneSnapshots(2)

      destination.removals shouldBe 0
      destination.contents.keySet.toSet shouldBe before
      destination.get(metadataKey).get.sameElements(metadata) shouldBe true
    }

    test(s"unreadable retained manifest $retainedIndex prevents pruning and metadata changes") {
      val (destination, snapshots) = seededRetention()
      destination.contents(Base16.encode(retentionFixture.roots(retainedIndex))) = Array.emptyByteArray
      val before = destination.contents.keySet.toSet
      val metadata = destination.get(metadataKey).get

      snapshots.pruneSnapshots(2)

      destination.removals shouldBe 0
      destination.contents.keySet.toSet shouldBe before
      destination.get(metadataKey).get.sameElements(metadata) shouldBe true
    }
  }

  test("a manifest referenced at a retained height is not removed at an obsolete height") {
    val (destination, snapshots) = seededRetention()
    val root = retentionFixture.roots.head
    snapshots.writeSnapshotsInfo(snapshots.readSnapshotsInfo.withNewManifest(40,
      scorex.crypto.hash.Digest32 @@ root)).get

    snapshots.pruneSnapshots(1)

    snapshots.readSnapshotsInfo.availableManifests.keySet shouldBe Set(40)
    destination.get(root).isDefined shouldBe true
    subtreeIds(root).foreach(id => destination.contents.contains(id) shouldBe true)
  }

  test("retaining zero snapshots removes all advertised snapshots and their fragments") {
    val (destination, snapshots) = seededRetention()
    snapshots.pruneSnapshots(0)
    snapshots.readSnapshotsInfo.availableManifests shouldBe empty
    destination.contents.keySet shouldBe Set(Base16.encode(metadataKey))
  }

  test("retaining all snapshots leaves content and metadata unchanged") {
    val (destination, snapshots) = seededRetention()
    val metadata = destination.get(metadataKey).get
    snapshots.pruneSnapshots(3)
    destination.removals shouldBe 0
    destination.get(metadataKey).get.sameElements(metadata) shouldBe true
  }
}
