package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage.{ScanPrefixArray, SecretPathsKey, lastUsedScanIdKey}
import org.ergoplatform.nodeView.wallet.scanning.{ScanRequest, ScanSerializer, ScanWalletInteraction}
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, DerivationPathSerializer}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.db.LDBKVStore

class WalletStorageSpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with DBSpec {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import org.ergoplatform.wallet.utils.WalletGenerators._

  it should "add and read derivation paths" in {
    def addPath(store: LDBKVStore, storedPaths: Seq[DerivationPath], derivationPath: DerivationPath): Unit = {
      val updatedPaths = (storedPaths :+ derivationPath).toSet
      val toInsert = Ints.toByteArray(updatedPaths.size) ++ updatedPaths
        .foldLeft(Array.empty[Byte]) { case (acc, path) =>
          val bytes = DerivationPathSerializer.toBytes(path)
          acc ++ Ints.toByteArray(bytes.length) ++ bytes
        }
      store.insert(SecretPathsKey, toInsert).get
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
        pubKeys.foreach(storage.addPublicKey(_).get)
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
        externalScanReqs.foreach(req => storage.addScan(req))
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

  it should "always increase ids" in {
    forAll(externalScanReqGen) { externalScanReq =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val scan = storage.addScan(externalScanReq).get

        storage.lastUsedScanId shouldBe scan.scanId

        storage.removeScan(scan.scanId).get
        storage.lastUsedScanId shouldBe scan.scanId

        val scan2 = storage.addScan(externalScanReq).get
        storage.lastUsedScanId shouldBe scan2.scanId
        storage.lastUsedScanId shouldBe (scan.scanId +1)
      }
    }
  }

  it should "read legacy Short scans and transition seamlessly across Short.MaxValue into Long scan ids" in {
    forAll(externalScanReqGen) { externalScanReq =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val legacyMaxShortId = ScanId @@ Short.MaxValue.toLong
        val legacyScan = externalScanReq.toScan(legacyMaxShortId).get
        // Simulate legacy 2-byte Short storage for scan key and lastUsedScanIdKey
        store.insert(
          Array(ScanPrefixArray ++ Shorts.toByteArray(Short.MaxValue), lastUsedScanIdKey),
          Array(ScanSerializer.toBytes(legacyScan), Shorts.toByteArray(Short.MaxValue))
        ).get

        storage.lastUsedScanId shouldBe Short.MaxValue.toLong
        storage.getScan(Short.MaxValue.toLong) shouldBe Some(legacyScan)

        val nextScan = storage.addScan(externalScanReq).get
        nextScan.scanId shouldBe (Short.MaxValue.toLong + 1L)
        storage.lastUsedScanId shouldBe (Short.MaxValue.toLong + 1L)
        storage.getScan(nextScan.scanId) shouldBe Some(nextScan)
        storage.allScans.map(_.scanId) shouldBe Seq(legacyMaxShortId, nextScan.scanId)
      }
    }
  }

}
