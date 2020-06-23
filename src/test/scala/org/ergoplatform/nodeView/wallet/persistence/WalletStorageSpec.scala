package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.scanning.ScanRequest
import org.ergoplatform.utils.generators.WalletGenerators
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scorex.testkit.utils.FileUtils

class WalletStorageSpec
  extends FlatSpec
    with Matchers
    with WalletGenerators
    with GeneratorDrivenPropertyChecks
    with DBSpec
    with FileUtils {

  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  it should "add and read derivation paths" in {
    forAll(Gen.nonEmptyListOf(derivationPathGen)) { paths =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        paths.foreach(storage.addPath)
        storage.readPaths() should contain theSameElementsAs paths.toSet
      }
    }
  }

  it should "add and read public keys" in {
    forAll(extendedPubKeyListGen) { pubKeys =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        pubKeys.foreach(storage.addKey)
        val keysRead = storage.readAllKeys()
        keysRead.length shouldBe pubKeys.length
        keysRead should contain theSameElementsAs pubKeys.toSet
      }
    }
  }

  it should "add, remove and read scans" in {
    forAll(Gen.nonEmptyListOf(externalAppReqGen)) { externalAppReqs =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        externalAppReqs.foreach(storage.addScan)
        val storageApps = storage.allScans
        val storageRequests = storageApps.map(app => ScanRequest(app.scanName, app.trackingRule))
        storageRequests.foreach(r => externalAppReqs.contains(r) shouldBe true)
        storageApps.map(_.scanId).foreach(storage.removeScan)
        storage.allScans.length shouldBe 0
      }
    }
  }

}
