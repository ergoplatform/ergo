package org.ergoplatform.serialization

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransactionSerializer
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendNoncedBoxSerializer
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

  property("AnyoneCanSpendBoxGen serialization") {
    checkSerializationRoundtrip(anyoneCanSpendBoxGen, AnyoneCanSpendNoncedBoxSerializer)
  }

  property("AnyoneCanSpendTransactionGen serialization") {
    checkSerializationRoundtrip(anyoneCanSpendTransactionGen, AnyoneCanSpendTransactionSerializer)
  }

  property("ErgoSyncInfo serialization") {
    checkSerializationRoundtrip(ergoSyncInfoGen, ErgoSyncInfoSerializer)
  }

  property("ErgoHeader serialization") {
    checkSerializationRoundtrip(invalidHeaderGen, HeaderSerializer)
  }

  property("BlockTransactions serialization") {
    checkSerializationRoundtrip(blockTransactionsGen, BlockTransactionsSerializer)
  }

  property("ADProofs serialization") {
    checkSerializationRoundtrip(randomADProofsGen, ADProofSerializer)
  }

}
