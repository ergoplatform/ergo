package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.BoxCertainty
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
