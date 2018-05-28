package org.ergoplatform.serialization

import io.circe.parser._
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.TransactionIdsForHeader
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendNoncedBox
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.core.ModifierId
import scorex.crypto.encode.Base58

class JsonSerializationSpec extends ErgoPropertyTest {

  property("TransactionIdsForHeader should be converted into json correctly") {
    val modifierId = genBytesList(Constants.ModifierIdSize).sample.get
    val stringId = Base58.encode(modifierId)
    val Right(expected) = parse(s"""{ "ids" : ["$stringId"]}""")
    val data = TransactionIdsForHeader(ModifierId @@ Seq(modifierId))
    data.asJson shouldEqual expected
  }

  property("AnyoneCanSpendNoncedBox should be converted into json correctly") {
    val nonce: Long = scala.util.Random.nextLong()
    val value: Long = Gen.chooseNum(1, Long.MaxValue).sample.get
    val box = AnyoneCanSpendNoncedBox(nonce = nonce, value = value)
    val id = AnyoneCanSpendNoncedBox.idFromBox(nonce)
    val stringId = Base58.encode(id)
    val Right(expected) = parse(s"""{"id": "$stringId", "nonce" : $nonce, "value" : $value}""")
    box.asJson shouldEqual expected
  }

}
