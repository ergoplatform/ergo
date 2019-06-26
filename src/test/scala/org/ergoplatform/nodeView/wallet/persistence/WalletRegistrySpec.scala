package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{LSMStore, Store}
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.BoxCertainty
import org.ergoplatform.wallet.boxes.BoxCertainty.{Certain, Uncertain}
import org.scalacheck.Gen
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
  import org.ergoplatform.nodeView.wallet.IdUtils._

  def createStore: Store = new LSMStore(createTempDir)

  it should "read certain boxes" in {
    forAll(trackedBoxGen) { box =>
      val certainBox = box.copy(certainty = Certain, spendingHeightOpt = None, spendingTxIdOpt = None)
      val store = createStore
      putBox(certainBox).transact(store)
      val registry = new WalletRegistry(store)(settings.walletSettings)

      registry.readCertainUnspentBoxes shouldBe Seq(certainBox)
    }
  }

  it should "read uncertain boxes" in {
    forAll(trackedBoxGen) { box =>
      val uncertainBox = box.copy(certainty = Uncertain)
      val index = RegistryIndex(0, 0, Map.empty, Seq(encodedBoxId(uncertainBox.box.id)))
      val store = createStore
      putBox(uncertainBox).flatMap(_ => putIndex(index)).transact(store)
      val registry = new WalletRegistry(store)(settings.walletSettings)

      registry.readUncertainBoxes shouldBe Seq(uncertainBox)
    }
  }

  it should "read transactions" in {
    forAll(invalidErgoTransactionGen) { tx =>
      val store = createStore
      putTx(tx).transact(store)
      val registry = new WalletRegistry(store)(settings.walletSettings)

      registry.readTransactions shouldBe Seq(tx)
    }
  }

  it should "update historical boxes when `keepSpentBoxes = true`" in {
    val ws = settings.walletSettings.copy(keepSpentBoxes = true)
    val spendingHeight = 0
    forAll(Gen.nonEmptyListOf(trackedBoxGen), modifierIdGen) { (boxes, txId) =>
      val unspentBoxes = boxes.map(
        _.copy(spendingHeightOpt = None, spendingTxIdOpt = None, certainty = BoxCertainty.Certain))
      val transitedBoxes = unspentBoxes.map(
        _.copy(spendingHeightOpt = Some(spendingHeight), spendingTxIdOpt = Some(txId)))
      val store = createStore
      putBoxes(unspentBoxes).transact(store)
      val registry = new WalletRegistry(store)(ws)
      registry.processHistoricalBoxes(unspentBoxes.map(txId -> _), spendingHeight).transact(store)
      registry.readHistoricalBoxes.toList should contain theSameElementsAs transitedBoxes
    }
  }

}
