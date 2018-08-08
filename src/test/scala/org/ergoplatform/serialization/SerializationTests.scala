package org.ergoplatform.serialization

import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransactionSerializer, TransactionIdsForHeaderSerializer}
import org.ergoplatform.nodeView.history.ErgoSyncInfoSerializer
import org.ergoplatform.nodeView.state.ErgoStateContextSerializer
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import org.scalatest.Assertion
import scorex.core.serialization.Serializer

class SerializationTests extends ErgoPropertyTest with scorex.testkit.SerializationTests {

  def checkSerializationRoundtripAndSize[A <: ErgoNodeViewModifier](generator: Gen[A],
                                                                    serializer: Serializer[A]): Assertion = {
    forAll(generator) { b: A =>
      val recovered = serializer.parseBytes(serializer.toBytes(b)).get
      val bytes = serializer.toBytes(b)
      bytes shouldEqual serializer.toBytes(recovered)
      bytes.length shouldEqual recovered.size
    }
  }

  property("HeaderWithoutInterlinks serialization") {
    val serializer = HeaderSerializer
    forAll(invalidHeaderGen) { b: Header =>
      val recovered = serializer.parseBytes(serializer.bytesWithoutInterlinks(b)).get.copy(interlinks = b.interlinks)
      recovered shouldBe b

      val size = serializer.bytesWithoutInterlinks(b).length
      recovered.size shouldBe size
    }
  }

  property("Header serialization") {
    val serializer = HeaderSerializer
    forAll(invalidHeaderGen) { b: Header =>
      val recovered = serializer.parseBytes(b.bytes)
      recovered.get shouldBe b
      recovered.get.size shouldBe b.bytes.length
    }
  }

  property("ErgoStateContext serialization") {
    checkSerializationRoundtrip(ergoStateContextGen, ErgoStateContextSerializer)
  }

  property("ErgoBox serialization") {
    checkSerializationRoundtrip(ergoBoxGen, ErgoBoxSerializer)
  }

  property("ErgoTransactionGen serialization") {
    checkSerializationRoundtripAndSize(invalidErgoTransactionGen, ErgoTransactionSerializer)
  }

  property("ErgoSyncInfo serialization") {
    checkSerializationRoundtrip(ergoSyncInfoGen, ErgoSyncInfoSerializer)
  }

  property("ErgoHeader serialization") {
    checkSerializationRoundtripAndSize(invalidHeaderGen, HeaderSerializer)
  }

  property("BlockTransactions serialization") {
    checkSerializationRoundtripAndSize(invalidBlockTransactionsGen, BlockTransactionsSerializer)
  }

  property("ADProofs serialization") {
    checkSerializationRoundtripAndSize(randomADProofsGen, ADProofSerializer)
  }

  property("TransactionIdsForHeader serialization") {
    checkSerializationRoundtrip(transactionIdsForHeaderGen, TransactionIdsForHeaderSerializer)
  }

}
