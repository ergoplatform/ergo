package org.ergoplatform.nodeView.history.storage

import java.io.IOException
import org.ergoplatform.CriticalSystemException
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.db.LDBKVStore

import scala.collection.mutable.ArrayBuffer

class HistoryStorageFactorySpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings

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
}
