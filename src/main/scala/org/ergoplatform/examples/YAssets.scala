package org.ergoplatform.examples

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.ApiCodecs
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.{Base16, Base58}
import sigmastate.Values.LongConstant
import sigmastate.serialization.ValueSerializer

object YAssets extends App with ApiCodecs {
  val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)


  //
  println("liquidation script hash: " + Base58.encode(Blake2b256.hash("2z8svm6rQ6UBqmJRFA91iEaL1j7WGTt4RMNPPKQagrvtWVAX6m92AwajM6Zd229jWsWuwbVg6M75K151AhYF71z9GowFDijA5GFqaWUJoUqqB3Q5WDsvVJduYw5RbnXJnjKpAna5ya4NH8v9u6jxuYJPoFn2LxxTp1G8e2YDBUhHheS6241KouHaTx1sCi5U9JQQUh2xysseHGRGAwduDMgGQBJaNcu7jXF1crce5iY7")))

  println(Base16.encode(ValueSerializer.serialize(LongConstant(100))))

}
