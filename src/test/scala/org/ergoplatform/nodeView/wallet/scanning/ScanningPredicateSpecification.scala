package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.{ErgoBox, ErgoScriptPredef, P2PKAddress}
import org.ergoplatform.ErgoBox.R1
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import scorex.crypto.hash.Digest32
import sigmastate.Values.ByteArrayConstant
import scala.util.Random
import scala.language.implicitConversions

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

}
