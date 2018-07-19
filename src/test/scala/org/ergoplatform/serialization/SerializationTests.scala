package org.ergoplatform.serialization

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransactionSerializer, TransactionIdsForHeaderSerializer}
import org.ergoplatform.nodeView.history.ErgoSyncInfoSerializer
import org.ergoplatform.nodeView.state.ErgoStateContextSerializer
import org.ergoplatform.utils.ErgoPropertyTest

class SerializationTests extends ErgoPropertyTest with scorex.testkit.SerializationTests {

  property("Extension serialization") {
    val serializer = ExtensionSerializer
    forAll(extensionGen) { b: Extension =>
      val recovered = serializer.parseBytes(b.bytes)
      recovered.get shouldBe b
    }
  }

  property("HeaderWithoutInterlinks serialization") {
    val serializer = HeaderSerializer
    forAll(invalidHeaderGen) { b: Header =>
      val recovered = serializer.parseBytes(serializer.bytesWithoutInterlinks(b)).get.copy(interlinks = b.interlinks)
      recovered shouldBe b
    }
  }

  property("Header serialization") {
    val serializer = HeaderSerializer
    forAll(invalidHeaderGen) { b: Header =>
      val recovered = serializer.parseBytes(b.bytes)
      recovered.get shouldBe b
    }
  }

  property("ErgoStateContext serialization") {
    checkSerializationRoundtrip(ergoStateContextGen, ErgoStateContextSerializer)
  }

  property("ErgoBox serialization") {
    checkSerializationRoundtrip(ergoBoxGen, ErgoBoxSerializer)
  }

  property("ErgoTransactionGen serialization") {
    checkSerializationRoundtrip(invalidErgoTransactionGen, ErgoTransactionSerializer)
  }

  property("ErgoSyncInfo serialization") {
    checkSerializationRoundtrip(ergoSyncInfoGen, ErgoSyncInfoSerializer)
  }

  property("ErgoHeader serialization") {
    checkSerializationRoundtrip(invalidHeaderGen, HeaderSerializer)
  }

  property("BlockTransactions serialization") {
    checkSerializationRoundtrip(invalidBlockTransactionsGen, BlockTransactionsSerializer)
  }

  property("ADProofs serialization") {
    checkSerializationRoundtrip(randomADProofsGen, ADProofSerializer)
  }

  property("TransactionIdsForHeader serialization") {
    checkSerializationRoundtrip(transactionIdsForHeaderGen, TransactionIdsForHeaderSerializer)
  }
}
