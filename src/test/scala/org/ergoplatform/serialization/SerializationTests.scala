package org.ergoplatform.serialization

import org.ergoplatform.ErgoGenerators
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.serialization.{BytesSerializable, Serializer}

class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators with scorex.testkit.SerializationTests {


  property("AnyoneCanSpendBoxGen serialization") {
    checkSerializationRoundtrip(anyoneCanSpendBoxGen)
  }

  property("AnyoneCanSpendTransactionGen serialization") {
    checkSerializationRoundtrip(anyoneCanSpendTransactionGen)
  }

  property("ErgoSyncInfo serialization") {
    checkSerializationRoundtrip(ergoSyncInfoGen)
  }

  property("ErgoHeader serialization") {
    checkSerializationRoundtrip(ergoHeaderGen)
  }

  property("ErgoMinimalHeader serialization") {
    checkSerializationRoundtrip(ergoMinimalHeaderGen)
  }

  property("BlockTransactions serialization") {
    checkSerializationRoundtrip(blockTransactionsGen)
  }

}
