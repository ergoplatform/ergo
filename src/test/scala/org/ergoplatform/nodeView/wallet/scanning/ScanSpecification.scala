package org.ergoplatform.nodeView.wallet.scanning

import io.circe.Json
import org.ergoplatform.utils.ErgoCorePropertyTest

class ScanSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import ScanJsonCodecs._

  property("external scan req json serialization roundtrip") {
    forAll(externalScanReqGen) { req =>
      val j: Json = scanReqEncoder.apply(req)
      scanReqDecoder.decodeJson(j).toTry.get == req
    }
  }

  property("external scan json serialization roundtrip") {
    forAll(externalAppGen) { req =>
      val j: Json = scanEncoder.apply(req)
      scanDecoder.decodeJson(j).toTry.get == req
    }
  }

}
