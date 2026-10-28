package scorex.db

import com.google.common.primitives.Longs
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

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
        reopened.lastVersionID.map(_.toSeq) shouldBe Some(latestVersion.toSeq)
        reopened.get(entryKey).map(_.toSeq) shouldBe Some(latestValue.toSeq)
        reopened.versionIdExists(firstVersion) shouldBe true
        reopened.rollbackTo(firstVersion).get
        reopened.get(entryKey).map(_.toSeq) shouldBe Some(firstValue.toSeq)
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
      reopened.lastVersionID.map(_.toSeq) shouldBe Some(latestVersion.toSeq)
      reopened.get(entryKey).map(_.toSeq) shouldBe Some(latestValue.toSeq)
      reopened.versionIdExists(firstVersion) shouldBe false
    } finally {
      reopened.close()
    }
  }
}
