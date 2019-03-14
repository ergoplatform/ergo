package org.ergoplatform.serialization

import org.ergoplatform.nodeView.wallet.TrackedBoxSerializer.TransactionLookup
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.WalletGenerators

import scala.util.Success

class TrackedBoxSerializationSpec extends ErgoPropertyTest with WalletGenerators {

  property("TrackedBox serialization") {
    forAll(trackedBoxGen) { box =>
      val serializer = new TrackedBoxSerializer(txLookup(box))
      val bytes = serializer.toBytes(box)
      val recovered = serializer.parseBytes(bytes)
      recovered shouldBe box
    }
  }

  def txLookup(box: TrackedBox): TransactionLookup = {
    if (box.spendingTx.nonEmpty) {
      Map(box.creationTxId -> box.creationTx, box.spendingTxId.value -> box.spendingTx.value).get(_)
    } else {
      Map(box.creationTxId -> box.creationTx).get(_)
    }
  }

}
