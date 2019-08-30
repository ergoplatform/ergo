package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.{ErgoBox, ErgoScriptPredef, P2PKAddress}
import org.ergoplatform.ErgoBox.R1
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.ErgoTransactionGenerators

import scala.util.Random

class ScanningPredicateSpecification extends ErgoPropertyTest with ErgoTransactionGenerators {

  val testDelay = 720 // to construct mining rewards scripts

  property("equals - p2pk") {
    forAll(ergoAddressGen){p2pkAddress =>
      //look for exact p2pk script bytes
      val box = ErgoBox(value = 1, p2pkAddress.script, creationHeight = 0)
      val scriptBytes = p2pkAddress.script.bytes
      EqualsScanningPredicate(R1, scriptBytes).filter(box) shouldBe true

      //then change random byte
      val scriptBytesModified = scriptBytes.clone()
      val idx = Random.nextInt(scriptBytesModified.length)
      scriptBytesModified.update(idx, ((scriptBytesModified(idx) + 1) % Byte.MaxValue).toByte)
      EqualsScanningPredicate(R1, scriptBytesModified).filter(box) shouldBe false

      //skip first byte in the filter
      EqualsScanningPredicate(R1, scriptBytes.tail).filter(box) shouldBe false
    }
  }

  property("equals - miner prop") {
    forAll(proveDlogGen){pk =>
      val minerProp = ErgoScriptPredef.rewardOutputScript(testDelay, pk)
      val mpBytes = minerProp.bytes

      //look for exact miner script bytes
      val box = ErgoBox(value = 1, minerProp, creationHeight = 0)
      EqualsScanningPredicate(R1, mpBytes).filter(box) shouldBe true

      //then change random byte
      val scriptBytesModified = mpBytes.clone()
      val idx = Random.nextInt(scriptBytesModified.length)
      scriptBytesModified.update(idx, ((scriptBytesModified(idx) + 1) % Byte.MaxValue).toByte)
      EqualsScanningPredicate(R1, scriptBytesModified).filter(box) shouldBe false
    }
  }

  property("contains - p2pk") {
    forAll(ergoAddressGen){p2pkAddress =>
      //look for exact p2pk script bytes
      val box = ErgoBox(value = 1, p2pkAddress.script, creationHeight = 0)
      val pkBytes = p2pkAddress.asInstanceOf[P2PKAddress].pubkeyBytes
      ContainsScanningPredicate(R1, pkBytes).filter(box) shouldBe true

      //then change random byte in filter
      val pkBytesModified = pkBytes.clone()
      val idx = Random.nextInt(pkBytesModified.length)
      pkBytesModified.update(idx, ((pkBytesModified(idx) + 1) % Byte.MaxValue).toByte)
      EqualsScanningPredicate(R1, pkBytesModified).filter(box) shouldBe false

      //skip first byte in the proper filter
      ContainsScanningPredicate(R1, pkBytes.tail).filter(box) shouldBe true
    }
  }

  property("contains - miner prop") {
    forAll(ergoAddressGen){p2pkAddress =>
      //look for exact p2pk script bytes
      val minerProp = ErgoScriptPredef.rewardOutputScript(testDelay, p2pkAddress.asInstanceOf[P2PKAddress].pubkey)
      val box = ErgoBox(value = 1, minerProp, creationHeight = 0)
      val pkBytes = p2pkAddress.asInstanceOf[P2PKAddress].pubkeyBytes

      ContainsScanningPredicate(R1, pkBytes).filter(box) shouldBe true

      //then change random byte in filter
      val pkBytesModified = pkBytes.clone()
      val idx = Random.nextInt(pkBytesModified.length)
      pkBytesModified.update(idx, ((pkBytesModified(idx) + 1) % Byte.MaxValue).toByte)
      EqualsScanningPredicate(R1, pkBytesModified).filter(box) shouldBe false

      //skip first byte in the proper filter
      ContainsScanningPredicate(R1, pkBytes.tail).filter(box) shouldBe true
    }
  }

}
