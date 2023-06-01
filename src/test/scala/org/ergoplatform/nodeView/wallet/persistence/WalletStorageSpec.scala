package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.Ints
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage.SecretPathsKey
import org.ergoplatform.nodeView.wallet.scanning.{LegacyScan, ScanRequest, ScanWalletInteraction}
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.secrets.{DerivationPath, DerivationPathSerializer}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.db.LDBKVStore

class WalletStorageSpec
  extends AnyFlatSpec
    with Matchers
    with WalletGenerators
    with ScalaCheckPropertyChecks
    with DBSpec {

  it should "add and read derivation paths" in {
    def addPath(store: LDBKVStore, storedPaths: Seq[DerivationPath], derivationPath: DerivationPath): Unit = {
      val updatedPaths = (storedPaths :+ derivationPath).toSet
      val toInsert = Ints.toByteArray(updatedPaths.size) ++ updatedPaths
        .foldLeft(Array.empty[Byte]) { case (acc, path) =>
          val bytes = DerivationPathSerializer.toBytes(path)
          acc ++ Ints.toByteArray(bytes.length) ++ bytes
        }
      store.insert(Array(SecretPathsKey -> toInsert)).get
    }

    forAll(Gen.nonEmptyListOf(derivationPathGen)) { paths =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        paths.foreach(path => addPath(store, storage.readPaths(), path))
        storage.readPaths() should contain theSameElementsAs paths.toSet
      }
    }
  }

  it should "add and read public keys" in {
    forAll(extendedPubKeyListGen) { pubKeys =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        pubKeys.foreach(storage.addPublicKeys(_).get)
        val keysRead = storage.readAllKeys()
        keysRead.length shouldBe pubKeys.length
        keysRead should contain theSameElementsAs pubKeys.toSet
      }
    }
  }

  it should "add, remove and read scans" in {
    forAll(Gen.nonEmptyListOf(externalScanReqGen)) { externalScanReqs =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        externalScanReqs.foreach(req => storage.addScanRequest(req))
        val storageApps = storage.allScans
        val storageRequests = storageApps.map { app =>
          ScanRequest(app.scanName, app.trackingRule, Some(ScanWalletInteraction.Off), Some(true))
        }
        storageRequests.foreach(r => externalScanReqs.contains(r) shouldBe true)
        storageApps.map(_.scanId).foreach(storage.removeScan(_).get)
        storage.allScans.length shouldBe 0
      }
    }
  }

  it should "create and remove legacy scans" in {
    forAll(Gen.nonEmptyListOf(externalAppGen)) { scans =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        scans.foreach { scan =>
          val legacyScan =
            LegacyScan(scan.scanId.toShort,
              scan.scanName,
              scan.trackingRule,
              scan.walletInteraction,
              scan.removeOffchain
            )
          storage.addLegacyScan(legacyScan).get
        }
        storage.getLegacyScans shouldNot be(empty)
        storage.getLegacyScans.foreach { scan =>
          storage.removeLegacyScan(scan.scanId).get
        }
        scans.foreach(storage.addScan)
        storage.getLegacyScans shouldBe empty
      }
    }
  }

  it should "always increase ids" in {
    forAll(externalScanReqGen) { externalScanReq =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val scan = storage.addScanRequest(externalScanReq).get

        storage.lastUsedScanId shouldBe scan.scanId

        storage.removeScan(scan.scanId).get
        storage.lastUsedScanId shouldBe scan.scanId

        val scan2 = storage.addScanRequest(externalScanReq).get
        storage.lastUsedScanId shouldBe scan2.scanId
        storage.lastUsedScanId shouldBe (scan.scanId + 1)
      }
    }
  }

}
