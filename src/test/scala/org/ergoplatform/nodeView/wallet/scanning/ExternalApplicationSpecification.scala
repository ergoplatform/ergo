package org.ergoplatform.nodeView.wallet.scanning

import io.circe.Json
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.WalletGenerators

class ExternalApplicationSpecification extends ErgoPropertyTest with WalletGenerators {
  import ExternalApplicationJsonCodecs._

  property("external app req json serialization roundtrip") {
    forAll(externalAppReqGen) { req =>
      val j: Json = appReqEncoder.apply(req)
      appReqDecoder.decodeJson(j).toTry.get == req
    }
  }

  property("external app json serialization roundtrip") {
    forAll(externalAppGen) { req =>
      val j: Json = appEncoder.apply(req)
      appDecoder.decodeJson(j).toTry.get == req
    }
  }

}
