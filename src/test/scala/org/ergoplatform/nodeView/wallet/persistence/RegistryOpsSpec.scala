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

  import RegistryOps._

  def createStore: Store = new LSMStore(createTempDir)

  property("putBox/getBox/updateBox/removeBox") {
    forAll(trackedBoxGen) { tb =>
      withVersionedStore(10) { store =>
        putBox(tb).transact(store)
        getBox(tb.box.id).transact(store) shouldBe Some(tb)
        val updatedBox = tb.copy(certainty = BoxCertainty.Certain, spendingHeightOpt = Some(0))
        updateBox(tb.box.id)(_ => updatedBox).transact(store)
        getBox(tb.box.id).transact(store) shouldBe Some(updatedBox)
        removeBoxes(Seq(tb.box.id)).transact(store)
        getBox(tb.box.id).transact(store) shouldBe None
      }
    }
  }

  property("putBoxes/getBoxes/updateBoxes/removeBoxes") {
    forAll(Gen.listOf(trackedBoxGen)) { tbs =>
      withVersionedStore(10) { store =>
        putBoxes(tbs).transact(store)
        getBoxes(tbs.map(_.box.id)).transact(store) should contain theSameElementsAs tbs.map(Some.apply)
        val updateFn = (tb: TrackedBox) => tb.copy(certainty = BoxCertainty.Certain, spendingHeightOpt = Some(0))
        val updatedBoxes = tbs.map(updateFn)
        updateBoxes(tbs.map(_.box.id))(updateFn).transact(store)
        getBoxes(tbs.map(_.box.id)).transact(store) should contain theSameElementsAs updatedBoxes.map(Some.apply)
        removeBoxes(tbs.map(_.box.id)).transact(store)
        getBoxes(tbs.map(_.box.id)).transact(store).flatten shouldBe Seq()
      }
    }
  }

  property("putTx/getTx/getAllTxs/removeTxs") {
    forAll(walletTransactionGen) { wtx =>
      withVersionedStore(10) { store =>
        putTx(wtx).transact(store)
        getTx(wtx.id).transact(store) shouldEqual Some(wtx)
        getAllTxs.transact(store) shouldEqual Seq(wtx)
        removeTxs(Seq(wtx.id)).transact(store)
        getAllTxs.transact(store) should not contain wtx
      }
    }
  }

  property("putTxs/getAllTxs") {
    forAll(Gen.listOf(walletTransactionGen)) { wtxs =>
      withVersionedStore(10) { store =>
        putTxs(wtxs).transact(store)
        getAllTxs.transact(store) should contain theSameElementsAs wtxs
      }
    }
  }

  property("putIndex/getIndex/updateIndex") {
    forAll(registryIndexGen) { index =>
      withVersionedStore(10) { store =>
        putIndex(index).transact(store)
        getIndex.transact(store) shouldBe index
        val updatedIndex = index.copy(height = 0, balance = 0)
        updateIndex(_ => updatedIndex).transact(store)
        getIndex.transact(store) shouldBe updatedIndex
      }
    }
  }

}
