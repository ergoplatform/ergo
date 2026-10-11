package org.ergoplatform.nodeView.history.storage

import java.io.{File, IOException}
import org.ergoplatform.CriticalSystemException
import org.ergoplatform.db.DBSpec
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.iq80.leveldb.Options
import scorex.db.{LDBFactory, LDBKVStore}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.util.control.ControlThrowable

class HistoryStorageFactorySpec extends ErgoCorePropertyTest with DBSpec {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

  for (failedAcquisition <- Seq(2, 3)) {
    property(s"factory closes acquired stores in reverse order when acquisition $failedAcquisition fails") {
      val closed = ArrayBuffer.empty[Int]
      var created = 0
      val failure = new IOException("classified store acquisition failure")
      val cleanup = new IOException("classified acquired-store cleanup failure")
      def create(path: String): LDBKVStore = {
        created += 1
        if (created == failedAcquisition) throw failure
        val id = created
        new LDBKVStore(null) {
          override def close(): Unit = {
            closed += id
            if (id == 1) throw cleanup
          }
        }
      }
      intercept[IOException](HistoryStorage.open(settings, create)) shouldBe failure
      closed.toSeq shouldBe (1 until failedAcquisition).reverse
      failure.getSuppressed.toSeq shouldBe Seq(cleanup)
    }
  }

  property("factory closes every acquired store after initialization failure and preserves cleanup errors") {
    val closed = ArrayBuffer.empty[Int]
    var created = 0
    val failure = new IOException("classified journal read failure")
    val extraCleanup = new IOException("classified extra-store cleanup failure")
    val objectCleanup = new IOException("classified object-store cleanup failure")
    def create(path: String): LDBKVStore = {
      created += 1
      val id = created
      new LDBKVStore(null) {
        override def get(key: K): Option[V] = throw failure
        override def close(): Unit = {
          closed += id
          if (id == 3) throw extraCleanup
          if (id == 2) throw objectCleanup
        }
      }
    }
    val error = intercept[CriticalSystemException](HistoryStorage.open(settings, create))
    error.getCause shouldBe failure
    closed.toSeq shouldBe Seq(3, 2, 1)
    error.getSuppressed.toSeq shouldBe Seq(extraCleanup, objectCleanup)
  }

  for (kind <- Seq("interruption", "control")) {
    property(s"factory closes every store when replaying a durable journal ends with $kind") {
      val root = createTempDir
      val localSettings = settings.copy(directory = root.getPath)
      val indexPath = new File(root, "history/index")
      indexPath.mkdirs()
      val header = defaultHeaderGen.sample.get
      val indexKey = Array.fill[Byte](32)(42)
      val intent = HistoryInsertionJournal.Intent(
        Vector(header.serializedId -> HistoryModifierSerializer.toBytes(header)),
        Vector(indexKey -> Array[Byte](7))
      )
      val prepared = LDBFactory.createKvDb(indexPath.getPath)
      try prepared.insert(HistoryInsertionJournal.key, HistoryInsertionJournal.encode(intent)).get
      finally prepared.close()

      val cause: Throwable = if (kind == "interruption") new InterruptedException("replay write interrupted")
        else new ControlThrowable {}
      val opened = ArrayBuffer.empty[LDBKVStore]
      val closed = ArrayBuffer.empty[String]
      def create(path: String): LDBKVStore = {
        val dir = new File(path)
        dir.mkdirs()
        val role = dir.getName
        val store = new LDBKVStore(LDBFactory.factory.open(dir, new Options().createIfMissing(true))) {
          private var wasClosed = false
          override def updateDurable(keys: Array[K], values: Array[V], removals: Array[K]): Try[Unit] = {
            val result = super.updateDurable(keys, values, removals)
            if (role == "objects") {
              result.get
              throw cause
            }
            result
          }
          override def close(): Unit = if (!wasClosed) {
            wasClosed = true
            closed += role
            super.close()
          }
        }
        opened += store
        store
      }
      try {
        intercept[Throwable](HistoryStorage.open(localSettings, create)) shouldBe cause
        closed.toSeq shouldBe Seq("extra", "objects", "index")
      } finally opened.reverseIterator.foreach(_.close())

      val recovered = HistoryStorage(localSettings)
      try {
        recovered.contains(header.id) shouldBe true
        recovered.getIndex(scorex.db.ByteArrayWrapper(indexKey)).get.toSeq shouldBe Seq[Byte](7)
      } finally recovered.close()
    }
  }

  property("factory continues cleanup when one close throws a control throwable") {
    val closed = ArrayBuffer.empty[Int]
    var created = 0
    val failure = new IOException("journal read failed")
    val cleanup = new ControlThrowable {}
    def create(path: String): LDBKVStore = {
      created += 1
      val id = created
      new LDBKVStore(null) {
        override def get(key: K): Option[V] = throw failure
        override def close(): Unit = {
          closed += id
          if (id == 3) throw cleanup
        }
      }
    }
    val error = intercept[CriticalSystemException](HistoryStorage.open(settings, create))
    error.getCause shouldBe failure
    closed.toSeq shouldBe Seq(3, 2, 1)
    error.getSuppressed.toSeq shouldBe Seq(cleanup)
  }

  for (failingStore <- Seq(3, 1, 2); kind <- Seq("io", "control")) {
    property(s"normal close attempts every store after $kind failure in store $failingStore") {
      val closed = ArrayBuffer.empty[Int]
      val primary: Throwable = if (kind == "io") new IOException("history store close failed")
        else new ControlThrowable {}
      def store(id: Int): LDBKVStore = new LDBKVStore(null) {
        override def close(): Unit = {
          closed += id
          if (id == failingStore) throw primary
        }
      }

      val storage = new HistoryStorage(store(1), store(2), store(3), settings.cacheSettings)
      intercept[Throwable](storage.close()) shouldBe primary
      closed.toSeq shouldBe Seq(3, 1, 2)
      primary.getSuppressed.toSeq shouldBe empty
    }
  }

  property("normal close preserves the first error and suppresses a later close error") {
    val closed = ArrayBuffer.empty[Int]
    val primary = new IOException("extra store close failed")
    val secondary = new ControlThrowable {}
    def store(id: Int): LDBKVStore = new LDBKVStore(null) {
      override def close(): Unit = {
        closed += id
        if (id == 3) throw primary
        if (id == 1) throw secondary
      }
    }

    val storage = new HistoryStorage(store(1), store(2), store(3), settings.cacheSettings)
    intercept[Throwable](storage.close()) shouldBe primary
    closed.toSeq shouldBe Seq(3, 1, 2)
    primary.getSuppressed.toSeq shouldBe Seq(secondary)
  }

  property("normal close releases real LevelDB locks after one post-close failure") {
    val root = createTempDir
    val localSettings = settings.copy(directory = root.getPath)
    val primary = new IOException("extra store reported a close error")
    val opened = ArrayBuffer.empty[LDBKVStore]
    def create(path: String): LDBKVStore = {
      val dir = new File(path)
      dir.mkdirs()
      val role = dir.getName
      val store = new LDBKVStore(LDBFactory.factory.open(dir, new Options().createIfMissing(true))) {
        private var wasClosed = false
        override def close(): Unit = if (!wasClosed) {
          wasClosed = true
          super.close()
          if (role == "extra") throw primary
        }
      }
      opened += store
      store
    }

    try {
      val storage = HistoryStorage.open(localSettings, create)
      intercept[IOException](storage.close()) shouldBe primary
      val registry = LDBFactory.factory.asInstanceOf[scorex.db.StoreRegistry]
      Seq("extra", "index", "objects").foreach { role =>
        val dir = new File(s"${localSettings.directory}/history/$role")
        registry.map.contains(dir) shouldBe false
        // Bypass the registry, which would otherwise return an existing handle.
        val db = registry.factory.open(dir, new Options().createIfMissing(false))
        try db.get(Array.fill[Byte](32)(51)) shouldBe null
        finally db.close()
      }
    } finally opened.reverseIterator.foreach(store => Try(store.close()))
  }
}
