package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.Store
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.db.DBSpec
import org.ergoplatform.utils.generators.WalletGenerators
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scorex.testkit.utils.FileUtils
import scorex.db.LDBVersionedStore

class WalletStorageSpec
  extends FlatSpec
    with Matchers
    with WalletGenerators
    with GeneratorDrivenPropertyChecks
    with DBSpec
    with FileUtils {

  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  def createStore: Store = new LDBVersionedStore(createTempDir)

  it should "add and read tracked addresses" in {
    forAll(ergoAddressGen) { address =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        storage.addTrackedAddress(address)
        storage.readTrackedAddresses shouldBe Seq(address)
      }
    }
  }

  it should "process postponed block correctly" in {
    forAll(postponedBlockGen) { block =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        storage.putBlock(block)
        storage.readBlocks(block.height, block.height) shouldBe Seq(block)
        storage.readLatestPostponedBlockHeight shouldBe Some(block.height)
        storage.removeBlock(block.height)
        storage.readBlocks(block.height, block.height) shouldBe Seq.empty
      }
    }
  }

  it should "add and read derivation paths" in {
    forAll(Gen.nonEmptyListOf(derivationPathGen)) { paths =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        paths.foreach(storage.addPath)
        storage.readPaths should contain theSameElementsAs paths.toSet
      }
    }
  }

}
