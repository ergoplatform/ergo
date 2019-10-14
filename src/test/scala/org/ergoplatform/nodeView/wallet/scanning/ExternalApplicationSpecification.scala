package org.ergoplatform.nodeView.wallet.scanning

import io.circe.Json
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.ErgoTransactionGenerators

class ExternalApplicationSpecification extends ErgoPropertyTest with ErgoTransactionGenerators {
  import ExternalApplicationJsonCodecs._
  import ScanningPredicateTestVectors._

  property("external app req json serialization roundtrip") {
    val req = ExternalAppRequest("Example application", complexOr)
    val j: Json = appReqEncoder.apply(req)
    appReqDecoder.decodeJson(j).toTry.get == req
  }

  property("external app json serialization roundtrip") {
    val req = ExternalApplication(2, "Example application", complexAnd)
    val j: Json = appEncoder.apply(req)
    appDecoder.decodeJson(j).toTry.get == req
  }

}
