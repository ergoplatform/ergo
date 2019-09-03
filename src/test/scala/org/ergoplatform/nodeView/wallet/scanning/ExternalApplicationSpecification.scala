package org.ergoplatform.nodeView.wallet.scanning

import io.circe.parser._
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

  property("external app json serialization - wrong id") {
    val j: Json = parse("""{
                          |  "appId" : -2,
                          |  "appName" : "Example application",
                          |  "trackingRule" : {
                          |    "predicate" : "and",
                          |    "args" : [
                          |      {
                          |        "predicate" : "contains",
                          |        "register" : "R1",
                          |        "bytes" : "0101010101010101010101010101010101010101010101010101010101010101"
                          |      },
                          |      {
                          |        "predicate" : "equals",
                          |        "register" : "R4",
                          |        "bytes" : "0101010101010101010101010101010101010101010101010101010101010101"
                          |      },
                          |      {
                          |        "predicate" : "containsAsset",
                          |        "assetId" : "0101010101010101010101010101010101010101010101010101010101010101"
                          |      }
                          |    ]
                          |  }
                          |}""".stripMargin).toTry.get
    appDecoder.decodeJson(j).toTry.isFailure shouldBe true
  }

}
