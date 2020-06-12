package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Shorts, Ints}
import org.ergoplatform.wallet.Constants.{ApplicationId, PaymentsAppId}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.IdUtils.EncodedBoxId
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.TrackedBox
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

  private val emptyBag = KeyValuePairsBag.empty
  private val walletBoxStatus = Set(PaymentsAppId)

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
      forAll(modifierIdGen) { txId =>
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
        registry.walletUnspentBoxes().toList should contain theSameElementsAs unspentBoxes
      }
    }
  }

  private def outputsSpentTest(keepSpent: Boolean): Unit = forAll(Gen.nonEmptyListOf(trackedBoxGen)) { boxes =>
    withHybridStore(10) { store =>
      val fakeTxId = modifierIdGen.sample.get
      val registry = new WalletRegistry(store)(settings.walletSettings.copy(keepSpentBoxes = keepSpent))
      val blockId = modifierIdGen.sample.get
      val outs = boxes.map { bx =>
        bx.copy(spendingHeightOpt = None, spendingTxIdOpt = None, applicationStatuses = walletBoxStatus)
      }
      val inputs = outs.map(tb => (fakeTxId, EncodedBoxId @@ tb.boxId, tb))
      registry.updateOnBlock(outs, inputs, Seq.empty)(blockId, 100)
      registry.walletUnspentBoxes() shouldBe Seq.empty
    }
  }

  it should "updateOnBlock() in correct way - outputs spent" in {
    outputsSpentTest(keepSpent = false)
    outputsSpentTest(keepSpent = true)
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

  it should "putBox/removeBox - 2 versions" in {
    forAll(trackedBoxGen) { tb =>
      withHybridStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        val tb1 = tb.copy(spendingHeightOpt = None, spendingTxIdOpt = None)
        val bag1 = WalletRegistry.putBox(emptyBag, tb1)

        val tb2 = tb.copy(spendingHeightOpt = Some(5000), spendingTxIdOpt = Some(modifierIdGen.sample.get))
        val bag2 = WalletRegistry.removeBox(bag1, tb1)
        WalletRegistry.putBox(bag2, tb2).transact(store)
        reg.getBox(tb.box.id) shouldBe Some(tb2)
        reg.walletUnspentBoxes() shouldBe Seq.empty
      }
    }
  }

  it should "putBoxes/getBoxes/removeBoxes" in {
    forAll(Gen.listOf(trackedBoxGen)) { tbs =>
      withHybridStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putBoxes(emptyBag, tbs).transact(store)
        reg.getBoxes(tbs.map(_.box.id)) should contain theSameElementsAs tbs.map(Some.apply)
        val updateFn = (tb: TrackedBox) => tb.copy(spendingHeightOpt = Some(0),
          applicationStatuses = Set(PaymentsAppId, ApplicationId @@ 2.toShort))
        val updatedBoxes = tbs.map(updateFn)
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
        reg.fetchDigest() shouldBe index
        val updatedIndex = index.copy(height = 0, walletBalance = 0)
        reg.updateDigest(emptyBag)(_ => updatedIndex).transact(store)
        reg.fetchDigest() shouldBe updatedIndex
      }
    }
  }

  it should "compose keys correctly" in {
    val box = trackedBoxGen.sample.get

    forAll { (prefix: Byte, appId: Short, height: Int, suffix: Byte) =>
      val key1 = (prefix +: Shorts.toByteArray(appId)) ++ Array.fill(32)(suffix)
      WalletRegistry.composeKey(prefix, ApplicationId @@ appId, suffix) shouldBe key1

      val key2 = (prefix +: Shorts.toByteArray(appId)) ++ Ints.toByteArray(height) ++ Array.fill(32)(suffix)
      WalletRegistry.composeKey(prefix, ApplicationId @@ appId, height, suffix) shouldBe key2

      val id = box.box.id
      val key3 = (prefix +: Shorts.toByteArray(appId)) ++ id
      WalletRegistry.composeKeyWithBoxId(prefix, ApplicationId @@ appId, id) shouldBe key3

      val key4 = (prefix +: Shorts.toByteArray(appId)) ++ Ints.toByteArray(height) ++ id
      WalletRegistry.composeKeyWithBoxId(prefix, ApplicationId @@ appId, height, id) shouldBe key4
    }
  }
}
