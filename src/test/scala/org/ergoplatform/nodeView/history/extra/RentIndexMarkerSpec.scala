package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{RentIndexEnabledBytes, RentIndexEnabledKey}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.util.bytesToId

/**
  * The rent index is written only while `storageRentCollection` is on. The marker-based
  * logic in `ErgoHistory.readOrGenerate` must rebuild the extra index on an off -> on
  * transition (blocks indexed while the flag was off have no rent rows) and must keep
  * it on an on -> off transition.
  */
class RentIndexMarkerSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}

  private val probeKey: Array[Byte] = Algos.hash("probe row")

  private def settingsFor(dir: String, rentCollection: Boolean): ErgoSettings =
    baseSettings.copy(
      directory = dir,
      nodeSettings = baseSettings.nodeSettings.copy(
        extraIndex = true,
        storageRentCollection = rentCollection))

  private def openHistory(dir: String, rentCollection: Boolean): ErgoHistory =
    ErgoHistory.readOrGenerate(settingsFor(dir, rentCollection))(null)

  private def writeProbe(dir: String): Unit = {
    val hs = HistoryStorage(settingsFor(dir, rentCollection = false))
    try hs.insertExtra(Array(probeKey -> Array[Byte](7)), Array.empty)
    finally hs.close()
  }

  private def readProbe(dir: String): Option[Array[Byte]] = {
    val hs = HistoryStorage(settingsFor(dir, rentCollection = false))
    try hs.modifierBytesById(bytesToId(probeKey))
    finally hs.close()
  }

  private def markerPresent(dir: String): Boolean = {
    val hs = HistoryStorage(settingsFor(dir, rentCollection = false))
    try hs.modifierBytesById(bytesToId(RentIndexEnabledKey)).exists(_.sameElements(RentIndexEnabledBytes))
    finally hs.close()
  }

  property("off -> on transition rebuilds the extra index, on -> off keeps it") {
    val dir = createTempDir.getAbsolutePath

    // first start with the flag on: the fresh index gets the schema and the rent marker
    var history = openHistory(dir, rentCollection = true)
    history.closeStorage()
    markerPresent(dir) shouldBe true

    // simulate indexed content with a probe row
    writeProbe(dir)
    readProbe(dir) shouldBe defined

    // restart with the flag off: the marker is cleared, the index is kept
    history = openHistory(dir, rentCollection = false)
    history.closeStorage()
    markerPresent(dir) shouldBe false
    readProbe(dir) shouldBe defined

    // restart with the flag on again: blocks indexed while the flag was off have no
    // rent rows, so the index is rebuilt (probe row gone) and the marker is set again
    history = openHistory(dir, rentCollection = true)
    history.closeStorage()
    markerPresent(dir) shouldBe true
    readProbe(dir) shouldBe None
  }
}
