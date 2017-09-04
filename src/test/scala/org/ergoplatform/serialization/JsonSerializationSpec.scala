package org.ergoplatform.serialization

import org.ergoplatform.modifiers.mempool.TransactionIdsForHeader
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import io.circe.parser._
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendNoncedBox
import org.scalacheck.Gen

class JsonSerializationSpec extends PropSpec with ErgoGenerators with Matchers {

  property("TransactionIdsForHeader should be converted into json correctly") {
    val modifierId = genBytesList(Constants.ModifierIdSize).sample.get
    val stringId = Base58.encode(modifierId)
    val Right(expected) = parse(s"""{ "ids" : ["$stringId"]}""")
    val data = TransactionIdsForHeader(Seq(modifierId))
    data.json shouldEqual expected
  }

  property("AnyoneCanSpendNoncedBox should be converted into json correctly") {
    val nonce: Long = scala.util.Random.nextLong()
    val value: Long = Gen.chooseNum(1, Long.MaxValue).sample.get
    val box = AnyoneCanSpendNoncedBox(nonce = nonce, value = value)
    val id = AnyoneCanSpendNoncedBox.idFromBox(nonce)
    val stringId = Base58.encode(id)
    val Right(expected) = parse(s"""{"id": "$stringId", "nonce" : $nonce, "value" : $value}""")
    box.json shouldEqual expected
  }

}
