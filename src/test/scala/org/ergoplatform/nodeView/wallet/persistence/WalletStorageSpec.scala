package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.utils.generators.WalletGenerators
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scorex.testkit.utils.FileUtils

class WalletStorageSpec
  extends FlatSpec
    with Matchers
    with WalletGenerators
    with GeneratorDrivenPropertyChecks
    with FileUtils {

  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  def createStore: Store = new LSMStore(createTempDir)

  it should "add and read tracked addresses" in {
    forAll(trackedAddressGen) { address =>
      val store = new WalletStorage(createStore, settings)
      store.addTrackedAddress(address)
      store.readTrackedAddresses shouldBe Seq(address)
    }
  }

  it should "process postponed block correctly" in {
    forAll(postponedBlockGen) { block =>
      val store = new WalletStorage(createStore, settings)
      store.putBlock(block)
      store.readBlocks(block.height, block.height) shouldBe Seq(block)
      store.readLatestPostponedBlockHeight shouldBe Some(block.height)
      store.removeBlock(block.height)
      store.readBlocks(block.height, block.height) shouldBe Seq.empty
    }
  }

}
