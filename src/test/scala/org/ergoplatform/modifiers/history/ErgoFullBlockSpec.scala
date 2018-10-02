package org.ergoplatform.modifiers.history

import io.circe.Json
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.utils.ErgoPropertyTest
import io.circe.syntax._
import org.scalatest.TryValues

class ErgoFullBlockSpec extends ErgoPropertyTest with TryValues {

  property("JSON serialization/deserialization") {

    val (st, bh) = createUtxoState()

    val block: ErgoFullBlock = validFullBlock(parentOpt = None, st, bh)

    val blockJson: Json = block.asJson

    val blockDecoded: ErgoFullBlock = blockJson.as[ErgoFullBlock].toTry.get

    blockDecoded shouldEqual block
  }
}
