package org.ergoplatform.serialization

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.{AnyoneCanSpendTransactionSerializer, TransactionIdsForHeaderSerializer}
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBoxSerializer, AnyoneCanSpendPropositionSerializer}
import org.ergoplatform.nodeView.history.ErgoSyncInfoSerializer
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators with scorex.testkit.SerializationTests {

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
      println(b)
      val recovered = serializer.parseBytes(b.bytes)
      recovered.get shouldBe b
    }
  }

  property("AnyoneCanSpendBoxGen serialization") {
    checkSerializationRoundtrip(anyoneCanSpendBoxGen, AnyoneCanSpendNoncedBoxSerializer)
  }

  property("AnyoneCanSpendPropositionBoxGen serialization") {
    checkSerializationRoundtrip(anyoneCanSpendProposition, AnyoneCanSpendPropositionSerializer)

    AnyoneCanSpendPropositionSerializer.Length shouldEqual 1
    AnyoneCanSpendPropositionSerializer.ByteValue shouldEqual Array.fill(1)(-127: Byte)
  }

  property("AnyoneCanSpendTransactionGen serialization") {
    checkSerializationRoundtrip(invalidAnyoneCanSpendTransactionGen, AnyoneCanSpendTransactionSerializer)
  }

  property("AnyoneCanSpendTransactionGen serialization - .bytes") {
    checkSerializationRoundtrip(invalidAnyoneCanSpendTransactionGen, AnyoneCanSpendTransactionSerializer)
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

  property("BlockTransactions serialization - .bytes") {
    checkSerializationRoundtrip(invalidBlockTransactionsGen, BlockTransactionsSerializer)
  }

  property("ADProofs serialization") {
    checkSerializationRoundtrip(randomADProofsGen, ADProofSerializer)
  }

  property("TransactionIdsForHeader serialization - .bytes") {
    checkSerializationRoundtrip(transactionIdsForHeaderGen, TransactionIdsForHeaderSerializer)
  }
}
