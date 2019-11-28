package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.wallet.Constants.PaymentsAppId
import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.{BoxCertainty, TrackedBox}
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
  val walletBoxStatus = Map(PaymentsAppId -> BoxCertainty.Certain)

  private val ws = settings.walletSettings

  it should "read unspent wallet boxes" in {
    forAll(trackedBoxGen) { box =>
      withHybridStore(10) { store =>
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
        withHybridStore(10) { store =>
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
      withHybridStore(10) { store =>
        WalletRegistry.putTx(emptyBag, wtx).transact(store)
        val registry = new WalletRegistry(store)(settings.walletSettings)

        registry.allWalletTxs() shouldBe Seq(wtx)
      }
    }
  }

  it should "update historical boxes when `keepSpentBoxes = true`" in {
    val ws = settings.walletSettings.copy(keepSpentBoxes = true)
    val spendingHeight = 0
    forAll(Gen.nonEmptyListOf(trackedBoxGen), modifierIdGen) { (boxes, txId) =>
      withHybridStore(10) { store =>
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

  it should "updateOnBlock() in correct way - only outputs" in {
    forAll(Gen.nonEmptyListOf(trackedBoxGen)) { boxes =>
      withHybridStore(10) { store =>
        val registry = new WalletRegistry(store)(settings.walletSettings)
        val blockId = modifierIdGen.sample.get
        val unspentBoxes = boxes.map(bx => bx.copy(spendingHeightOpt = None, spendingTxIdOpt = None, applicationStatuses = walletBoxStatus))
        registry.updateOnBlock(unspentBoxes, Seq.empty, Seq.empty)(blockId, 100)
        registry.walletUnspentBoxes().toList  should contain theSameElementsAs unspentBoxes
      }
    }
  }


  it should "putBox/getBox/removeBox" in {
    forAll(trackedBoxGen) { tb =>
      withHybridStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putBox(emptyBag, tb).transact(store)
        reg.getBox(tb.box.id) shouldBe Some(tb)
        WalletRegistry.removeBoxes(emptyBag, Seq(tb)).transact(store)
        reg.getBox(tb.box.id) shouldBe None
      }
    }
  }

  it should "putBoxes/getBoxes/updateBoxes/removeBoxes" in {
    forAll(Gen.listOf(trackedBoxGen)) { tbs =>
      withHybridStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putBoxes(emptyBag, tbs).transact(store)
        reg.getBoxes(tbs.map(_.box.id)) should contain theSameElementsAs tbs.map(Some.apply)
        val updateFn = (tb: TrackedBox) => tb.copy(spendingHeightOpt = Some(0),
          applicationStatuses = Map(1.toShort -> BoxCertainty.Certain, 2.toShort -> BoxCertainty.Uncertain))
        val updatedBoxes = tbs.map(updateFn)
        reg.updateBoxes(emptyBag, tbs.map(_.box.id))(updateFn).transact(store)
        reg.getBoxes(tbs.map(_.box.id)) should contain theSameElementsAs updatedBoxes.map(Some.apply)
        WalletRegistry.removeBoxes(emptyBag, tbs).transact(store)
        reg.getBoxes(tbs.map(_.box.id)).flatten shouldBe Seq()
      }
    }
  }

  it should "putTx/getTx/getAllTxs/removeTxs" in {
    forAll(walletTransactionGen) { wtx =>
      withHybridStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putTx(emptyBag, wtx).transact(store)
        reg.getTx(wtx.id) shouldEqual Some(wtx)
        reg.allWalletTxs() shouldEqual Seq(wtx)
        WalletRegistry.removeTxs(emptyBag, Seq(wtx.id)).transact(store)
        reg.allWalletTxs() should not contain wtx
      }
    }
  }

  it should "putTxs/getAllTxs" in {
    forAll(Gen.listOf(walletTransactionGen)) { wtxs =>
      withHybridStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putTxs(emptyBag, wtxs).transact(store)
        reg.allWalletTxs() should contain theSameElementsAs wtxs
      }
    }
  }

  it should "putIndex/digest/updateIndex" in {
    forAll(registrySummaryGen) { index =>
      withHybridStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putDigest(emptyBag, index).transact(store)
        reg.getDigest() shouldBe index
        val updatedIndex = index.copy(height = 0, walletBalance = 0)
        reg.updateDigest(emptyBag)(_ => updatedIndex).transact(store)
        reg.getDigest() shouldBe updatedIndex
      }
    }
  }

}
