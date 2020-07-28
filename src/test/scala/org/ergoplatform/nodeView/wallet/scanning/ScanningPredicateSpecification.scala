package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.{ErgoBox, ErgoScriptPredef, P2PKAddress}
import org.ergoplatform.ErgoBox.R1
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import scorex.crypto.hash.Digest32
import sigmastate.Values.ByteArrayConstant

import scala.util.Random
import scala.language.implicitConversions
import io.circe.parser._
import org.ergoplatform.wallet.serialization.JsonCodecsWrapper

class ScanningPredicateSpecification extends ErgoPropertyTest with ErgoTransactionGenerators {

  val testDelay = 720 // to construct mining rewards scripts

  private implicit def bacFromBytes(bs: Array[Byte]) = ByteArrayConstant(bs)

  //helper function to change random byte
  private def mutateRandomByte(source: Array[Byte]): Array[Byte] = {
    val sourceModified = source.clone()
    val idx = Random.nextInt(sourceModified.length)
    sourceModified.update(idx, ((sourceModified(idx) + 1) % Byte.MaxValue).toByte)
    sourceModified
  }

  property("equals - p2pk") {
    forAll(ergoAddressGen) { p2pkAddress =>
      //look for exact p2pk script bytes
      val box = ErgoBox(value = 1, p2pkAddress.script, creationHeight = 0)
      val scriptBytes = p2pkAddress.script.bytes
      EqualsScanningPredicate(R1, scriptBytes).filter(box) shouldBe true

      //then change random byte
      val scriptBytesModified = mutateRandomByte(scriptBytes)
      EqualsScanningPredicate(R1, scriptBytesModified).filter(box) shouldBe false

      //skip first byte in the filter
      EqualsScanningPredicate(R1, scriptBytes.tail).filter(box) shouldBe false
    }
  }

  property("equals - miner prop") {
    forAll(proveDlogGen) { pk =>
      val minerProp = ErgoScriptPredef.rewardOutputScript(testDelay, pk)
      val mpBytes = minerProp.bytes

      //look for exact miner script bytes
      val box = ErgoBox(value = 1, minerProp, creationHeight = 0)
      EqualsScanningPredicate(R1, mpBytes).filter(box) shouldBe true

      //then change random byte
      val scriptBytesModified = mutateRandomByte(mpBytes)
      EqualsScanningPredicate(R1, scriptBytesModified).filter(box) shouldBe false
    }
  }

  property("contains - p2pk") {
    forAll(ergoAddressGen) { p2pkAddress =>
      //look for exact p2pk script bytes
      val box = ErgoBox(value = 1, p2pkAddress.script, creationHeight = 0)
      val pkBytes = p2pkAddress.asInstanceOf[P2PKAddress].pubkeyBytes
      ContainsScanningPredicate(R1, pkBytes).filter(box) shouldBe true

      //then change random byte in filter
      val pkBytesModified = mutateRandomByte(pkBytes)
      EqualsScanningPredicate(R1, pkBytesModified).filter(box) shouldBe false

      //skip first byte in the proper filter
      ContainsScanningPredicate(R1, pkBytes.tail).filter(box) shouldBe true
    }
  }

  property("contains - miner prop") {
    forAll(ergoAddressGen) { p2pkAddress =>
      //look for exact p2pk script bytes
      val minerProp = ErgoScriptPredef.rewardOutputScript(testDelay, p2pkAddress.asInstanceOf[P2PKAddress].pubkey)
      val box = ErgoBox(value = 1, minerProp, creationHeight = 0)
      val pkBytes = p2pkAddress.asInstanceOf[P2PKAddress].pubkeyBytes

      ContainsScanningPredicate(R1, pkBytes).filter(box) shouldBe true

      //then change random byte in filter
      val pkBytesModified = mutateRandomByte(pkBytes)
      EqualsScanningPredicate(R1, pkBytesModified).filter(box) shouldBe false

      //skip first byte in the proper filter
      ContainsScanningPredicate(R1, pkBytes.tail).filter(box) shouldBe true
    }
  }

  property("containsAsset") {
    forAll(proveDlogGen) { pk =>
      forAll(assetGen) { case (tokenId, amt) =>
        val box = ErgoBox(value = 1, pk, creationHeight = 0, additionalTokens = Seq(tokenId -> amt))
        ContainsAssetPredicate(tokenId).filter(box) shouldBe true

        val emptyBox = ErgoBox(value = 1, pk, creationHeight = 0)
        ContainsAssetPredicate(tokenId).filter(emptyBox) shouldBe false

        ContainsAssetPredicate(Digest32 @@ mutateRandomByte(tokenId)).filter(box) shouldBe false
      }
    }
  }

  property("and") {
    forAll(ergoAddressGen) { p2pk =>
      forAll(assetGen) { case (tokenId, amt) =>
        val box = ErgoBox(value = 1, p2pk.script, creationHeight = 0, additionalTokens = Seq(tokenId -> amt))

        //box contains both asset and p2pk script
        AndScanningPredicate(ContainsAssetPredicate(tokenId), ContainsScanningPredicate(R1, p2pk.contentBytes))
          .filter(box) shouldBe true

        AndScanningPredicate(
          EqualsScanningPredicate(R1, p2pk.script.bytes),
          ContainsScanningPredicate(R1, p2pk.contentBytes)
        ).filter(box) shouldBe true

        AndScanningPredicate(
          ContainsAssetPredicate(Digest32 @@ mutateRandomByte(tokenId)),
          ContainsScanningPredicate(R1, p2pk.contentBytes)
        ).filter(box) shouldBe false

        AndScanningPredicate(
          ContainsAssetPredicate(tokenId),
          ContainsScanningPredicate(R1, mutateRandomByte(p2pk.contentBytes))
        ).filter(box) shouldBe false
      }
    }
  }

  property("or") {
    forAll(ergoAddressGen) { p2pk =>
      forAll(assetGen) { case (tokenId, amt) =>
        val box = ErgoBox(value = 1, p2pk.script, creationHeight = 0, additionalTokens = Seq(tokenId -> amt))

        //box contains both asset and p2pk script
        OrScanningPredicate(ContainsAssetPredicate(tokenId), ContainsScanningPredicate(R1, p2pk.contentBytes))
          .filter(box) shouldBe true

        OrScanningPredicate(
          EqualsScanningPredicate(R1, p2pk.script.bytes),
          ContainsScanningPredicate(R1, p2pk.contentBytes)
        ).filter(box) shouldBe true

        OrScanningPredicate(
          ContainsAssetPredicate(Digest32 @@ mutateRandomByte(tokenId)),
          ContainsScanningPredicate(R1, p2pk.contentBytes)
        ).filter(box) shouldBe true

        OrScanningPredicate(
          ContainsAssetPredicate(tokenId),
          ContainsScanningPredicate(R1, mutateRandomByte(p2pk.contentBytes))
        ).filter(box) shouldBe true

        OrScanningPredicate(
          ContainsAssetPredicate(Digest32 @@ mutateRandomByte(tokenId)),
          ContainsScanningPredicate(R1, mutateRandomByte(p2pk.contentBytes))
        ).filter(box) shouldBe false
      }
    }
  }

  property("and - complex case #1 - with deserialization"){
    val scanString =
      """
        |{
        |    "scanId": 80,
        |    "scanName": "Local Oracle Datapoint Scan",
        |    "trackingRule": {
        |      "predicate": "and",
        |      "args": [
        |        {
        |          "predicate": "containsAsset",
        |          "assetId": "12caaacb51c89646fac9a3786eb98d0113bd57d68223ccc11754a4f67281daed"
        |        },
        |        {
        |          "predicate": "equals",
        |          "register": "R1",
        |          "value": "0edf03100604000400050004000e20b662db51cf2dc39f110a021c2a31c74f0a1a18ffffbf73e8a051a7b8c0f09ebc0eca02100e040004000e2012caaacb51c89646fac9a3786eb98d0113bd57d68223ccc11754a4f67281daed0500040004140580dac4090402048092f401040201010402058092f4010400d804d601b2a5730000d602b5db6501fed9010263ed93e4c67202050ec5a7938cb2db63087202730100017302d603b17202d6049db072027303d9010441639a8c720401e4c68c72040206057e720305ea02d1edededededededed93c27201e4c6a7060e917203730493db63087201db6308a793e4c672010405720493e4c6720105049ae4c6a70504730592c17201730693e4c672010405720492c1720199c1a77e9c9a720373077308058cb0720286027309730ad901053c400163d802d6078c720501d6088c72070186029a7208730beded8c72070293c2b2a5720800d0cde4c68c720502040792c1b2a5720800730c02b2ad7202d9010563cde4c672050407730d00d803d601b2a5730000d602e4c6a70407d603b2db6501fe730100ea02d1ededededed93e4c672010407720293e4c67201050ec5720391e4c672010605730293c27201c2a793db63087201db6308a7ed938cb2db6308720373030001730493c272037305cd7202"
        |        },
        |        {
        |          "predicate": "equals",
        |          "register": "R4",
        |          "value": "07029f2230dbe53f6b84d8a884a3407c3dffe43daf8037445441be7cdcd261feeaa4"
        |        }
        |      ]
        |    }
        |  }
      """.stripMargin

    val scanJson = parse(scanString).toOption.get
    val scan = ScanJsonCodecs.scanDecoder.decodeJson(scanJson).toOption.get

    val boxString =
      """
        |{
        |      "boxId": "9441cf85bb564c72426b5eca49bce5f6cb27778c598ad02f34ca4358027b3a44",
        |      "value": 2000000,
        |      "ergoTree": "100604000400050004000e20b662db51cf2dc39f110a021c2a31c74f0a1a18ffffbf73e8a051a7b8c0f09ebc0eca02100e040004000e2012caaacb51c89646fac9a3786eb98d0113bd57d68223ccc11754a4f67281daed0500040004140580dac4090402048092f401040201010402058092f4010400d804d601b2a5730000d602b5db6501fed9010263ed93e4c67202050ec5a7938cb2db63087202730100017302d603b17202d6049db072027303d9010441639a8c720401e4c68c72040206057e720305ea02d1edededededededed93c27201e4c6a7060e917203730493db63087201db6308a793e4c672010405720493e4c6720105049ae4c6a70504730592c17201730693e4c672010405720492c1720199c1a77e9c9a720373077308058cb0720286027309730ad901053c400163d802d6078c720501d6088c72070186029a7208730beded8c72070293c2b2a5720800d0cde4c68c720502040792c1b2a5720800730c02b2ad7202d9010563cde4c672050407730d00d803d601b2a5730000d602e4c6a70407d603b2db6501fe730100ea02d1ededededed93e4c672010407720293e4c67201050ec5720391e4c672010605730293c27201c2a793db63087201db6308a7ed938cb2db6308720373030001730493c272037305cd7202",
        |      "assets": [
        |        {
        |          "tokenId": "12caaacb51c89646fac9a3786eb98d0113bd57d68223ccc11754a4f67281daed",
        |          "amount": 1
        |        }
        |      ],
        |      "creationHeight": 280831,
        |      "additionalRegisters": {
        |        "R4": "07029f2230dbe53f6b84d8a884a3407c3dffe43daf8037445441be7cdcd261feeaa4",
        |        "R5": "0e0101"
        |      },
        |      "transactionId": "dfad960e6fb085f10e9b700f5631251b11209e7ed09a56c94d8bd91452226344",
        |      "index": 1
        |    }
      """.stripMargin

    val boxJson = parse(boxString).toOption.get
    val box = JsonCodecsWrapper.ergoBoxDecoder.decodeJson(boxJson).toOption.get

    scan.trackingRule.filter(box) shouldBe true
  }

}
