package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.db.DBSpec
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.BoxCertainty
import org.ergoplatform.wallet.boxes.BoxCertainty.{Certain, Uncertain}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scorex.testkit.utils.FileUtils
import scorex.db.LDBVersionedStore

class WalletRegistrySpec
  extends FlatSpec
    with Matchers
    with DBSpec
    with GeneratorDrivenPropertyChecks
    with WalletGenerators
    with FileUtils {

  import RegistryOps._
  import org.ergoplatform.nodeView.wallet.IdUtils._

  it should "read certain boxes" in {
    forAll(trackedBoxGen) { box =>
      withVersionedStore(10) { store =>
        val certainBox = box.copy(certainty = Certain, spendingHeightOpt = None, spendingTxIdOpt = None)
        putBox(certainBox).transact(store)
        val registry = new WalletRegistry(store)(settings.walletSettings)

        registry.readCertainUnspentBoxes shouldBe Seq(certainBox)
      }
    }
  }

  it should "read uncertain boxes" in {
    forAll(trackedBoxGen) { box =>
      withVersionedStore(10) { store =>
        val uncertainBox = box.copy(certainty = Uncertain)
        val index = RegistryIndex(0, 0, Map.empty, Seq(encodedBoxId(uncertainBox.box.id)))
        putBox(uncertainBox).flatMap(_ => putIndex(index)).transact(store)
        val registry = new WalletRegistry(store)(settings.walletSettings)

        registry.readUncertainBoxes shouldBe Seq(uncertainBox)
      }
    }
  }

  it should "read transactions" in {
    forAll(walletTransactionGen) { wtx =>
      withVersionedStore(10) { store =>
        putTx(wtx).transact(store)
        val registry = new WalletRegistry(store)(settings.walletSettings)

        registry.readTransactions shouldBe Seq(wtx)
      }
    }
  }

  it should "update historical boxes when `keepSpentBoxes = true`" in {
    val ws = settings.walletSettings.copy(keepSpentBoxes = true)
    val spendingHeight = 0
    forAll(Gen.nonEmptyListOf(trackedBoxGen), modifierIdGen) { (boxes, txId) =>
      withVersionedStore(10) { store =>
        val unspentBoxes = boxes.map(
          _.copy(spendingHeightOpt = None, spendingTxIdOpt = None, certainty = BoxCertainty.Certain))
        val transitedBoxes = unspentBoxes.map(
          _.copy(spendingHeightOpt = Some(spendingHeight), spendingTxIdOpt = Some(txId)))

        putBoxes(unspentBoxes).transact(store)
        val registry = new WalletRegistry(store)(ws)
        registry.processHistoricalBoxes(unspentBoxes.map(txId -> _), spendingHeight).transact(store)
        registry.readHistoricalBoxes.toList should contain theSameElementsAs transitedBoxes
      }
    }
  }

}
