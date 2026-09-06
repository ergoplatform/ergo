package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.Ints
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage.SecretPathsKey
import org.ergoplatform.nodeView.wallet.scanning.{ScanRequest, ScanWalletInteraction}
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, DerivationPathSerializer}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.db.LDBKVStore

import scala.util.{Failure, Try}

class WalletStorageSpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with DBSpec {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import org.ergoplatform.wallet.utils.WalletGenerators._

  private class ControlledStore(delegate: LDBKVStore) extends LDBKVStore(null) {
    var writeFailure: Option[Throwable] = None
    var beforeWrite: () => Unit = () => ()

    override def get(key: K): Option[V] = delegate.get(key)

    override def insert(key: K, value: V): Try[Unit] = {
      beforeWrite()
      writeFailure.fold(delegate.insert(key, value))(Failure(_))
    }
  }

  Seq(false, true).foreach { warmCache =>
    it should s"publish state context only after persistence with warm cache $warmCache" in {
      withStore { store =>
        val oldContext = ErgoStateContext.empty(settings.chainSettings,
          org.ergoplatform.utils.ErgoCoreTestConstants.parameters)
        val newContext = ErgoStateContext.empty(settings.chainSettings, extendedParameters)
        oldContext.bytes.toSeq should not be newContext.bytes.toSeq
        new WalletStorage(store, settings).updateStateContext(oldContext).get
        val controlled = new ControlledStore(store)
        val storage = new WalletStorage(controlled, settings)
        if (warmCache) {
          storage.getStateContext(extendedParameters).bytes.toSeq shouldBe oldContext.bytes.toSeq
        }

        val failure = new IllegalStateException("state context write rejected")
        controlled.writeFailure = Some(failure)
        storage.updateStateContext(newContext) shouldBe Failure(failure)
        storage.getStateContext(extendedParameters).bytes.toSeq shouldBe oldContext.bytes.toSeq
        new WalletStorage(store, settings).getStateContext(extendedParameters).bytes.toSeq shouldBe oldContext.bytes.toSeq

        controlled.writeFailure = None
        controlled.beforeWrite = () => {
          storage.getStateContext(extendedParameters).bytes.toSeq shouldBe oldContext.bytes.toSeq
        }
        storage.updateStateContext(newContext).get
        storage.getStateContext(extendedParameters) should be theSameInstanceAs newContext
        new WalletStorage(store, settings).getStateContext(extendedParameters).bytes.toSeq shouldBe newContext.bytes.toSeq
      }
    }
  }

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

}
