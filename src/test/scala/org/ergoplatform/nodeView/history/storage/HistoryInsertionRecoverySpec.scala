package org.ergoplatform.nodeView.history.storage

import java.io.{File, IOException}
import org.ergoplatform.CriticalSystemException
import org.ergoplatform.db.DBSpec
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.iq80.leveldb.{DB, Options}
import scorex.db.{ByteArrayWrapper, LDBFactory, LDBKVStore}

import scala.util.{Failure, Try}

class HistoryInsertionRecoverySpec extends ErgoCorePropertyTest with DBSpec {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

  private case class Fault(role: String, call: Int, afterWrite: Boolean, error: IOException)
  private class Store(db: DB, role: String, fault: Option[Fault], before: () => Unit) extends LDBKVStore(db) {
    private var calls = 0
    override def updateDurable(keys: Array[K], values: Array[V], removals: Array[K]): Try[Unit] = {
      calls += 1
      before()
      val selected = fault.filter(f => f.role == role && f.call == calls)
      if (selected.exists(!_.afterWrite)) Failure(selected.get.error)
      else super.updateDurable(keys, values, removals).flatMap { result =>
        selected.fold[Try[Unit]](scala.util.Success(result))(f => Failure(f.error))
      }
    }
  }

  private class Opened(root: File, fault: Option[Fault] = None, before: () => Unit = () => ()) {
    private def open(role: String): Store = {
      val path = new File(root, s"history/$role")
      path.mkdirs()
      new Store(LDBFactory.factory.open(path, new Options().createIfMissing(true)), role, fault, before)
    }
    val index = open("index")
    val objects = open("objects")
    val extra = open("extra")
    val storage = new HistoryStorage(index, objects, extra, settings.cacheSettings)
    def close(): Unit = storage.close()
  }

  private val indexKey = ByteArrayWrapper(Array.fill[Byte](32)(42))

  property("completed insertion preserves object formats, caches and ordinary factory reopen") {
    val root = createTempDir
    val opened = new Opened(root)
    val header = defaultHeaderGen.sample.get
    val bytes = HistoryModifierSerializer.toBytes(header)
    try {
      opened.storage.insert(Array(indexKey -> Array[Byte](7)), Array[BlockSection](header)).get
      opened.storage.modifierById(header.id).get.id shouldBe header.id
      opened.storage.get(header.id).get.toSeq shouldBe bytes.toSeq
      opened.storage.getIndex(indexKey).get.toSeq shouldBe Seq[Byte](7)
      opened.index.get(HistoryInsertionJournal.key) shouldBe None
    } finally opened.close()
    val reopened = HistoryStorage(settings.copy(directory = root.getPath))
    try {
      reopened.contains(header.id) shouldBe true
      reopened.getIndex(indexKey).get.toSeq shouldBe Seq[Byte](7)
    } finally reopened.close()
  }

  for ((role, call) <- Seq("index" -> 1, "objects" -> 1, "index" -> 2); after <- Seq(false, true)) {
    property(s"insertion recovers after $role write $call reports failure ${if (after) "after" else "before"} writing") {
      val root = createTempDir
      val cause = new IOException("classified durable write failure")
      val opened = new Opened(root, Some(Fault(role, call, after, cause)))
      val header = defaultHeaderGen.sample.get
      try {
        val error = opened.storage.insert(Array(indexKey -> Array[Byte](9)), Array[BlockSection](header)).failed.get
        error shouldBe a[CriticalSystemException]
        error.getCause shouldBe cause
        intercept[CriticalSystemException](opened.storage.contains(header.id))
        intercept[CriticalSystemException](opened.storage.get(header.id))
        intercept[CriticalSystemException](opened.storage.get(header.serializedId))
        intercept[CriticalSystemException](opened.storage.modifierBytesById(header.id))
        intercept[CriticalSystemException](opened.storage.modifierTypeAndBytesById(header.id))
        intercept[CriticalSystemException](opened.storage.modifierById(header.id))
        intercept[CriticalSystemException](opened.storage.getIndex(indexKey))
        intercept[CriticalSystemException](opened.storage.getExtraIndex(header.id))
        intercept[CriticalSystemException](opened.storage.insert(header.serializedId, Array[Byte](1)))
        intercept[CriticalSystemException](opened.storage.remove(Array.empty, Array.empty))
        intercept[CriticalSystemException](opened.storage.insertExtra(Array.empty, Array.empty))
        intercept[CriticalSystemException](opened.storage.removeExtra(Array.empty))
        opened.storage.insert(Array.empty[(ByteArrayWrapper, Array[Byte])], Array.empty[BlockSection]).isFailure shouldBe true
      } finally opened.close()
      val reopened = new Opened(root)
      try {
        val prepared = role != "index" || call != 1 || after
        reopened.storage.contains(header.id) shouldBe prepared
        reopened.storage.getIndex(indexKey).map(_.toSeq) shouldBe (if (prepared) Some(Seq[Byte](9)) else None)
        reopened.index.get(HistoryInsertionJournal.key) shouldBe None
        reopened.storage.insert(Array(indexKey -> Array[Byte](9)), Array[BlockSection](header)).get
      } finally reopened.close()
    }
  }

  property("a failed recovery preserves its cause and intent for the next ordinary reopen") {
    val root = createTempDir
    val header = defaultHeaderGen.sample.get
    val opened = new Opened(root, Some(Fault("objects", 1, false, new IOException("initial write"))))
    try opened.storage.insert(Array(indexKey -> Array[Byte](3)), Array[BlockSection](header)).isFailure shouldBe true
    finally opened.close()
    val failure = new IOException("recovery write")
    val recovering = new Opened(root, Some(Fault("objects", 1, false, failure)))
    try {
      intercept[CriticalSystemException](recovering.storage.contains(header.id)).getCause shouldBe failure
      recovering.index.get(HistoryInsertionJournal.key).isDefined shouldBe true
    } finally recovering.close()
    val recovered = HistoryStorage(settings.copy(directory = root.getPath))
    try {
      recovered.contains(header.id) shouldBe true
      recovered.getIndex(indexKey).get.toSeq shouldBe Seq[Byte](3)
    } finally recovered.close()
  }

  property("freezing failures precede preparation and caller-owned index arrays cannot change the committed values") {
    val root = createTempDir
    val key = Array.fill[Byte](32)(8)
    val value = Array[Byte](4)
    val opened = new Opened(root, before = () => { key(0) = 99; value(0) = 99 })
    val header = defaultHeaderGen.sample.get
    val expectedKey = ByteArrayWrapper(key.clone())
    try {
      opened.storage.insert(Array(indexKey -> Array[Byte](1)), Array[BlockSection](null)).isFailure shouldBe true
      opened.index.get(HistoryInsertionJournal.key) shouldBe None
      opened.storage.contains(header.id) shouldBe false
      opened.storage.insert(Array(ByteArrayWrapper(key) -> value), Array[BlockSection](header)).get
      opened.storage.getIndex(expectedKey).get.toSeq shouldBe Seq[Byte](4)
      opened.index.get(expectedKey.data).get.toSeq shouldBe Seq[Byte](4)
      opened.storage.modifierById(header.id).get.id shouldBe header.id
    } finally opened.close()
  }

  for (after <- Seq(false, true)) {
    property(s"recovery can be retried when final indexes fail ${if (after) "after" else "before"} application") {
      val root = createTempDir
      val header = defaultHeaderGen.sample.get
      val opened = new Opened(root, Some(Fault("objects", 1, false, new IOException("initial write"))))
      try opened.storage.insert(Array(indexKey -> Array[Byte](6)), Array[BlockSection](header)).isFailure shouldBe true
      finally opened.close()
      val failure = new IOException("recovery final indexes")
      val recovering = new Opened(root, Some(Fault("index", 1, after, failure)))
      try {
        intercept[CriticalSystemException](recovering.storage.contains(header.id)).getCause shouldBe failure
        intercept[CriticalSystemException](recovering.storage.getIndex(indexKey))
        recovering.index.get(HistoryInsertionJournal.key).isDefined shouldBe !after
      } finally recovering.close()
      val recovered = HistoryStorage(settings.copy(directory = root.getPath))
      try {
        recovered.contains(header.id) shouldBe true
        recovered.getIndex(indexKey).get.toSeq shouldBe Seq[Byte](6)
      } finally recovered.close()
    }
  }

  for (invalidBytes <- Seq(false, true)) {
    property(s"recovery rejects ${if (invalidBytes) "invalid object bytes" else "mismatched object identity"} before writing") {
      val root = createTempDir
      var writes = 0
      val opened = new Opened(root, before = () => writes += 1)
      val header = defaultHeaderGen.sample.get
      val key = header.serializedId.clone()
      if (!invalidBytes) key(0) = (key(0) ^ 1).toByte
      val bytes = if (invalidBytes) Array[Byte](0) else HistoryModifierSerializer.toBytes(header)
      val record = HistoryInsertionJournal.encode(HistoryInsertionJournal.Intent(
        Vector(key -> bytes), Vector(indexKey.data -> Array[Byte](1))))
      try {
        opened.index.insert(HistoryInsertionJournal.key, record).get
        val failure = intercept[CriticalSystemException](opened.storage.contains(header.id))
        if (!invalidBytes) failure.getCause.getMessage should include("object identity mismatch")
        writes shouldBe 0
        opened.objects.get(key) shouldBe None
        opened.index.get(indexKey.data) shouldBe None
        opened.index.get(HistoryInsertionJournal.key).get.toSeq shouldBe record.toSeq
      } finally opened.close()
    }
  }

  property("reserved journal keys are inaccessible through ordinary index APIs") {
    val opened = new Opened(createTempDir)
    val reserved = ByteArrayWrapper(HistoryInsertionJournal.key)
    try {
      HistoryInsertionJournal.key.length should not be 32
      opened.storage.insert(Array(reserved -> Array[Byte](1)), Array.empty[BlockSection]).isFailure shouldBe true
      intercept[IllegalArgumentException](opened.storage.getIndex(reserved))
      intercept[IllegalArgumentException](opened.storage.remove(Array(reserved), Array.empty))
      opened.index.get(HistoryInsertionJournal.key) shouldBe None
    } finally opened.close()
  }

  property("unknown journal versions fail opening without discarding the record") {
    val root = createTempDir
    val opened = new Opened(root)
    val encoded = HistoryInsertionJournal.encode(HistoryInsertionJournal.Intent(Vector.empty, Vector.empty))
    encoded(7) = 2
    val payload = encoded.dropRight(32)
    val record = payload ++ java.security.MessageDigest.getInstance("SHA-256").digest(payload)
    try opened.index.insert(HistoryInsertionJournal.key, record).get finally opened.close()
    intercept[CriticalSystemException](HistoryStorage(settings.copy(directory = root.getPath)))
    val inspected = new Opened(root)
    try inspected.index.get(HistoryInsertionJournal.key).get.toSeq shouldBe record.toSeq finally inspected.close()
  }
}
