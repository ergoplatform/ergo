package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.BoxCertainty.{Certain, Uncertain}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scorex.testkit.utils.FileUtils

class WalletRegistrySpec
  extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with WalletGenerators
    with FileUtils {

  import RegistryOps._

  def createStore: Store = new LSMStore(createTempDir)

  it should "read certain boxes" in {
    forAll(trackedBoxGen) { box =>
      val certainBox = box.copy(certainty = Certain)
      val store = createStore
      putBox(certainBox).transact(store)
      val registry = new WalletRegistry(store)

      registry.readCertainBoxes shouldBe Seq(certainBox)
    }
  }

  it should "read uncertain boxes" in {
    forAll(trackedBoxGen) { box =>
      val uncertainBox = box.copy(certainty = Uncertain)
      val index = RegistryIndex(0, 0, Seq.empty, Seq(uncertainBox.box.id))
      val store = createStore
      putBox(uncertainBox).flatMap(_ => putIndex(index)).transact(store)
      val registry = new WalletRegistry(store)

      registry.readUncertainBoxes shouldBe Seq(uncertainBox)
    }
  }

}
