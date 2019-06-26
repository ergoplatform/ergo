package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.{BoxCertainty, TrackedBox}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.utils.FileUtils

class RegistryOpsSpec
  extends PropSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with WalletGenerators
    with FileUtils {

  import RegistryOps._

  def createStore: Store = new LSMStore(createTempDir)

  property("putBox/getBox/updateBox/removeBox") {
    forAll(trackedBoxGen) { tb =>
      val store = createStore
      putBox(tb).transact(store)
      getBox(tb.box.id).transact(store) shouldBe Some(tb)
      val updatedBox = tb.copy(certainty = BoxCertainty.Certain, spendingHeightOpt = Some(0))
      updateBox(tb.box.id)(_ => updatedBox).transact(store)
      getBox(tb.box.id).transact(store) shouldBe Some(updatedBox)
      removeBoxes(Seq(tb.box.id)).transact(store)
      getBox(tb.box.id).transact(store) shouldBe None
    }
  }

  property("putBoxes/getBoxes/updateBoxes/removeBoxes") {
    forAll(Gen.listOf(trackedBoxGen)) { tbs =>
      val store = createStore
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

  property("putTx/getTx/removeTxs") {
    forAll(invalidErgoTransactionGen) { tx =>
      val store = createStore
      putTx(tx).transact(store)
      getTx(tx.id).transact(store) shouldBe Some(tx)
      removeTxs(Seq(tx.id)).transact(store)
      getTx(tx.id).transact(store) shouldBe None
    }
  }

  property("putTxs/getTxs") {
    forAll(Gen.listOf(invalidErgoTransactionGen)) { txs =>
      val store = createStore
      putTxs(txs).transact(store)
      getTxs(txs.map(_.id)).transact(store) should contain theSameElementsAs txs.map(Some.apply)
    }
  }

  property("putIndex/getIndex/updateIndex") {
    forAll(registryIndexGen) { index =>
      val store = createStore
      putIndex(index).transact(store)
      getIndex.transact(store) shouldBe index
      val updatedIndex = index.copy(height = 0, balance = 0)
      updateIndex(_ => updatedIndex).transact(store)
      getIndex.transact(store) shouldBe updatedIndex
    }
  }

}
