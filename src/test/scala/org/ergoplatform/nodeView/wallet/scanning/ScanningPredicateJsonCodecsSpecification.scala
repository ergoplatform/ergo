package org.ergoplatform.nodeView.wallet.scanning

import io.circe.parser._
import org.ergoplatform.ErgoBox
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16

class ScanningPredicateJsonCodecsSpecification extends ErgoPropertyTest with ErgoTransactionGenerators {

  import ScanningPredicateJsonCodecs.{scanningPredicateEncoder, scanningPredicateDecoder}

  property("complex or roundtrip") {
    val p = ScanningPredicateTestVectors.complexOr
    val j = scanningPredicateEncoder(p)
    scanningPredicateDecoder.decodeJson(j).toTry.get == p
  }

  property("complex and roundtrip") {
    val p = ScanningPredicateTestVectors.complexAnd
    val j = scanningPredicateEncoder(p)
    scanningPredicateDecoder.decodeJson(j).toTry.get == p
  }

  property("example from EIP-1"){
    val j = parse(
      """{"predicate": "and", "args":[{"predicate": "contains",
        |"bytes": "02dada811a888cd0dc7a0a41739a3ad9b0f427741fe6ca19700cf1a51200c96bf7"},
        |{"predicate": "containsAsset",
        |"assetId": "02dada811a888cd0dc7a0a41739a3ad9b0f427741fe6ca19700cf1a51200c96bf7"}]}""".stripMargin).toOption.get

    val bs = Base16.decode("02dada811a888cd0dc7a0a41739a3ad9b0f427741fe6ca19700cf1a51200c96bf7").get
    scanningPredicateDecoder.decodeJson(j).toTry.get == AndScanningPredicate(
      ContainsScanningPredicate(ErgoBox.R1, bs),
      ContainsAssetPredicate(Digest32 @@ bs)
    )
  }

  property("example from EIP-1 w.explicit register"){
    val j = parse(
      """{"predicate": "and", "args":[{"predicate": "contains", "register": "R4",
        |"bytes": "02dada811a888cd0dc7a0a41739a3ad9b0f427741fe6ca19700cf1a51200c96bf7"},
        |{"predicate": "containsAsset",
        |"assetId": "02dada811a888cd0dc7a0a41739a3ad9b0f427741fe6ca19700cf1a51200c96bf7"}]}""".stripMargin).toOption.get

    val bs = Base16.decode("02dada811a888cd0dc7a0a41739a3ad9b0f427741fe6ca19700cf1a51200c96bf7").get
    scanningPredicateDecoder.decodeJson(j).toTry.get == AndScanningPredicate(
      ContainsScanningPredicate(ErgoBox.R4, bs),
      ContainsAssetPredicate(Digest32 @@ bs)
    )
  }

}
