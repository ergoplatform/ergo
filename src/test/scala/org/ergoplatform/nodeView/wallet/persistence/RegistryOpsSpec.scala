package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.{BoxCertainty, TrackedBox}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scorex.testkit.utils.FileUtils

class RegistryOpsSpec
  extends PropSpec
    with Matchers
    with DBSpec
    with GeneratorDrivenPropertyChecks
    with WalletGenerators
    with FileUtils {


  def createStore: Store = new LSMStore(createTempDir)

  private val ws = settings.walletSettings
  private val emptyBag = KeyValuePairsBag.empty

  property("putBox/getBox/removeBox") {
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

  property("putBoxes/getBoxes/updateBoxes/removeBoxes") {
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

  property("putTx/getTx/getAllTxs/removeTxs") {
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

  property("putTxs/getAllTxs") {
    forAll(Gen.listOf(walletTransactionGen)) { wtxs =>
      withHybridStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putTxs(emptyBag, wtxs).transact(store)
        reg.allWalletTxs() should contain theSameElementsAs wtxs
      }
    }
  }

  property("putIndex/digest/updateIndex") {
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
