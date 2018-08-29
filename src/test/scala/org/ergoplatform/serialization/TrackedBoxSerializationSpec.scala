package org.ergoplatform.serialization

import org.ergoplatform.nodeView.wallet.TrackedBoxSerializer.TransactionLookup
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.utils.{ErgoPropertyTest, WalletGenerators}

import scala.util.Success

class TrackedBoxSerializationSpec extends ErgoPropertyTest with WalletGenerators {

  property("TrackedBox serialization") {
    forAll(trackedBoxGen) { box =>
      val serializer = new TrackedBoxSerializer(txLookup(box))
      val bytes = serializer.toBytes(box)
      val recovered = serializer.parseBytes(bytes)
      recovered shouldBe Success(box)
    }
  }

  def txLookup(box: TrackedBox): TransactionLookup = {
    if (box.spendingTxOpt.nonEmpty) {
      Map(box.creationTxId -> box.creationTx, box.spendingTxId.value -> box.spendingTxOpt.value).get(_)
    } else {
      Map(box.creationTxId -> box.creationTx).get(_)
    }
  }

}
