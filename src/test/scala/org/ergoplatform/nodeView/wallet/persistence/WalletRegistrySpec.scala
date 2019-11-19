package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.wallet.Constants.WalletAppId

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.BoxCertainty
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scorex.testkit.utils.FileUtils

class WalletRegistrySpec
  extends FlatSpec
    with Matchers
    with DBSpec
    with GeneratorDrivenPropertyChecks
    with WalletGenerators
    with FileUtils {


  def createStore: Store = new LSMStore(createTempDir)

  private val emptyBag = KeyValuePairsBag.empty
  val walletBoxStatus = Seq(WalletAppId -> BoxCertainty.Certain)

  it should "read unspent wallet boxes" in {
    forAll(trackedBoxGen) { box =>
      withVersionedStore(10) { store =>
        val unspentBox = box.copy(spendingHeightOpt = None, spendingTxIdOpt = None, applicationStatuses = walletBoxStatus)
        WalletRegistry.putBox(emptyBag, unspentBox).transact(store)

        val registry = new WalletRegistry(store)(settings.walletSettings)
        registry.walletUnspentBoxes() shouldBe Seq(unspentBox)
      }
    }
  }

  it should "read spent wallet boxes" in {
    forAll(trackedBoxGen) { box =>
      forAll(modifierIdGen) {txId =>
        withVersionedStore(10) { store =>
          val uncertainBox = box.copy(spendingHeightOpt = Some(10000), spendingTxIdOpt = Some(txId), applicationStatuses = walletBoxStatus)
          WalletRegistry.putBox(emptyBag, uncertainBox).transact(store)
          val registry = new WalletRegistry(store)(settings.walletSettings)
          registry.walletSpentBoxes() shouldBe Seq(uncertainBox)
        }
      }
    }
  }

  it should "read wallet transactions" in {
    forAll(walletTransactionGen) { wtx =>
      withVersionedStore(10) { store =>
        WalletRegistry.putTx(emptyBag, wtx).transact(store)
        val registry = new WalletRegistry(store)(settings.walletSettings)

        registry.getAllWalletTxs() shouldBe Seq(wtx)
      }
    }
  }

  it should "update historical boxes when `keepSpentBoxes = true`" in {
    val ws = settings.walletSettings.copy(keepSpentBoxes = true)
    val spendingHeight = 0
    forAll(Gen.nonEmptyListOf(trackedBoxGen), modifierIdGen) { (boxes, txId) =>
      withVersionedStore(10) { store =>
        val unspentBoxes = boxes.map(
          _.copy(spendingHeightOpt = None, spendingTxIdOpt = None, applicationStatuses = walletBoxStatus))
        val transitedBoxes = unspentBoxes.map(
          _.copy(spendingHeightOpt = Some(spendingHeight), spendingTxIdOpt = Some(txId)))

        WalletRegistry.putBoxes(emptyBag, unspentBoxes).transact(store)
        val registry = new WalletRegistry(store)(ws)
        registry.processHistoricalBoxes(emptyBag, unspentBoxes.map(txId -> _), spendingHeight).transact(store)
        registry.walletSpentBoxes().toList should contain theSameElementsAs transitedBoxes
      }
    }
  }

}
