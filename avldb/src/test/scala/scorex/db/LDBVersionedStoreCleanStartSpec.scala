package scorex.db

import com.google.common.primitives.Longs
import org.iq80.leveldb.WriteBatch
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.hash.Blake2b256

import java.io.File
import java.nio.file.Files
import scala.util.Try

class LDBVersionedStoreCleanStartSpec extends AnyPropSpec with Matchers {

  private val entryKey = Array[Byte](1)
  private val previousVersion = Longs.toByteArray(0L)
  private val firstVersion = Longs.toByteArray(1L)
  private val latestVersion = Longs.toByteArray(2L)
  private val firstValue = Array[Byte](11)
  private val latestValue = Array[Byte](22)
  private val cutpointMessage = "stopped after writing last_version_key"

  private def newStoreDir(): File =
    Files.createTempDirectory("avldb_cleanstart_").toFile

  private def seedPreviousMarker(dir: File): Unit = {
    val store = new LDBVersionedStore(dir, 0)
    try {
      store.update(previousVersion, Seq.empty, Seq(entryKey -> Array[Byte](0))).get
    } finally {
      store.close()
    }
  }

  private def addTwoVersions(store: LDBVersionedStore): Unit = {
    store.update(firstVersion, Seq.empty, Seq(entryKey -> firstValue)).get
    store.update(latestVersion, Seq.empty, Seq(entryKey -> latestValue)).get
  }

  private class StopBeforeUndoWrite(dir: File) extends LDBVersionedStore(dir, 100) {
    var reachedCutpoint = false

    override private[db] def writeCleanStartUndoBatch(batch: WriteBatch): Unit = {
      val marker = db.get(Blake2b256("last_version"))
      assert(marker != null && marker.sameElements(latestVersion),
        "the newest version must be written before undo records are removed")
      reachedCutpoint = true
      throw new IllegalStateException(cutpointMessage)
    }
  }

  for (hadPreviousMarker <- Seq(false, true); reopenKeepVersions <- Seq(0, 100)) {
    property(s"zero-retention cutpoint recovers with prior marker $hadPreviousMarker and reopen retention $reopenKeepVersions") {
      val dir = newStoreDir()
      if (hadPreviousMarker) seedPreviousMarker(dir)

      val interrupted = new StopBeforeUndoWrite(dir)
      try {
        val markerBefore = Option(interrupted.db.get(Blake2b256("last_version")))
        if (hadPreviousMarker) {
          markerBefore.exists(_.sameElements(previousVersion)) shouldBe true
        } else {
          markerBefore shouldBe None
        }
        addTwoVersions(interrupted)
        val error = intercept[IllegalStateException] {
          interrupted.setKeepVersions(0)
        }
        error.getMessage shouldBe cutpointMessage
        interrupted.reachedCutpoint shouldBe true
      } finally {
        interrupted.close()
      }

      val reopened = new LDBVersionedStore(dir, reopenKeepVersions)
      try {
        reopened.lastVersionID.get.sameElements(latestVersion) shouldBe true
        reopened(entryKey).sameElements(latestValue) shouldBe true
        if (reopenKeepVersions == 0) {
          reopened.versionIdExists(firstVersion) shouldBe false
        } else {
          reopened.versionIdExists(firstVersion) shouldBe true
          reopened.rollbackTo(firstVersion).get
          reopened(entryKey).sameElements(firstValue) shouldBe true
        }
      } finally {
        reopened.close()
      }
    }
  }

  for (hadPreviousMarker <- Seq(false, true)) {
    property(s"failed marker write keeps undo history with prior marker $hadPreviousMarker") {
      val dir = newStoreDir()
      if (hadPreviousMarker) seedPreviousMarker(dir)

      val interrupted = new LDBVersionedStore(dir, 100)
      try {
        addTwoVersions(interrupted)
        interrupted.db.close()
        intercept[Exception] {
          interrupted.setKeepVersions(0)
        }
      } finally {
        // The main DB was closed to make its marker write fail; close the undo DB too.
        Try(interrupted.close())
      }

      val reopened = new LDBVersionedStore(dir, 100)
      try {
        reopened.lastVersionID.get.sameElements(latestVersion) shouldBe true
        reopened(entryKey).sameElements(latestValue) shouldBe true
        reopened.versionIdExists(firstVersion) shouldBe true
        reopened.rollbackTo(firstVersion).get
        reopened(entryKey).sameElements(firstValue) shouldBe true
      } finally {
        reopened.close()
      }
    }
  }

  property("healthy zero-retention cleanup preserves the newest version") {
    val dir = newStoreDir()
    val store = new LDBVersionedStore(dir, 100)
    try {
      addTwoVersions(store)
      store.setKeepVersions(0) shouldBe 100
    } finally {
      store.close()
    }

    val reopened = new LDBVersionedStore(dir, 0)
    try {
      reopened.lastVersionID.get.sameElements(latestVersion) shouldBe true
      reopened(entryKey).sameElements(latestValue) shouldBe true
      reopened.versionIdExists(firstVersion) shouldBe false
    } finally {
      reopened.close()
    }
  }
}
