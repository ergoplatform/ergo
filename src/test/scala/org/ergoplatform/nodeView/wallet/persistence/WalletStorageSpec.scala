package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.db.DBSpec
import org.ergoplatform.utils.generators.WalletGenerators
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

  it should "add and read tracked addresses" in {
    forAll(trackedAddressGen) { address =>
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

}
