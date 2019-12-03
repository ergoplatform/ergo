package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.scanning.ExternalAppRequest
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

  def createStore: Store = new LSMStore(createTempDir)

  it should "add and read derivation paths" in {
    forAll(Gen.nonEmptyListOf(derivationPathGen)) { paths =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        paths.foreach(storage.addPath)
        storage.readPaths should contain theSameElementsAs paths.toSet
      }
    }
  }

  it should "add, remove and read applications" in {
    forAll(Gen.nonEmptyListOf(externalAppReqGen)) { externalAppReqs =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        externalAppReqs.foreach(storage.addApplication)
        val storageApps = storage.allApplications
        val storageRequests = storageApps.map(app => ExternalAppRequest(app.appName, app.trackingRule, app.alwaysCertain))
        storageRequests.foreach(r => externalAppReqs.contains(r) shouldBe true)
        storageApps.map(_.appId).foreach(storage.removeApplication)
        storage.allApplications.length shouldBe 0
      }
    }
  }

}
