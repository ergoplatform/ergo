package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.{ErgoPropertyTest, WalletGenerators}
import scorex.core.ModifierId

import scala.util.{Success, Try}

class TrackedBoxSerializationSpec extends ErgoPropertyTest with WalletGenerators {

  property("BoxUnspent should be serialized and deserialized to bytes") {
    forAll(boxUnspentGen) { box =>
      val serializer = new BoxUnspentSerializer(transactionLookup(box.tx))
      serializer.parseBytes(serializer.toBytes(box)) shouldEqual Success(box)
    }
  }

  property("BoxSpent should be serialized and deserialized to bytes") {
    forAll(boxSpentGen) { box =>
      val serializer = new BoxSpentSerializer(transactionLookup(box.parentTx, box.spendingTx))
      serializer.parseBytes(serializer.toBytes(box)) shouldEqual Success(box)
    }
  }

  private def transactionLookup(txs: ErgoTransaction *)(id: ModifierId): Option[ErgoTransaction] = {
    val txMap = txs.map { tx => ByteArrayWrapper(tx.id) -> tx }.toMap
    log.info(ByteArrayWrapper(id).toString)
    log.info(txMap.toString)
    txMap.get(ByteArrayWrapper(id))
  }
}
