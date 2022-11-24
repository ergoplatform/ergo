package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.encode.Base58
import org.ergoplatform.P2PKAddress
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.{ExtendedSecretKey, Index, DerivationPath}

class ExtendedSecretKeySpec
  extends AnyPropSpec
    with Matchers
    with ScalaCheckPropertyChecks {

  val seedStr = "edge talent poet tortoise trumpet dose"
  val seed: Array[Byte] = Mnemonic.toSeed(SecretString.create(seedStr))

  def equalBase58(v1: Array[Byte], v2b58: String): Assertion =
    Base58.encode(v1) shouldEqual v2b58

  property("key tree derivation from seed (test vectors from BIP32 check)") {
    val expectedRoot = "4rEDKLd17LX4xNR8ss4ithdqFRc3iFnTiTtQbanWJbCT"
    val cases = Seq(
      ("CLdMMHxNtiPzDnWrVuZQr22VyUx8deUG7vMqMNW7as7M", 1),
      ("9icjp3TuTpRaTn6JK6AHw2nVJQaUnwmkXVdBdQSS98xD", 2),
      ("DWMp3L9JZiywxSb5gSjc5dYxPwEZ6KkmasNiHD6VRcpJ", Index.hardIndex(2))
    )

    val root = ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation = false)

    equalBase58(root.keyBytes, expectedRoot)

    cases.foldLeft(root) { case (parent, (expectedKey, idx)) =>
      val child = parent.child(idx)
      equalBase58(child.keyBytes, expectedKey)
      child
    }
  }

  property("path derivation (test vectors from BIP32 check)") {
    val cases = Seq(
      ("CLdMMHxNtiPzDnWrVuZQr22VyUx8deUG7vMqMNW7as7M", "m/1"),
      ("9icjp3TuTpRaTn6JK6AHw2nVJQaUnwmkXVdBdQSS98xD", "m/1/2"),
      ("DWMp3L9JZiywxSb5gSjc5dYxPwEZ6KkmasNiHD6VRcpJ", "m/1/2/2'")
    )

    val root = ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation = false)

    cases.foreach { case (expectedKey, path) =>
      val derived = root.derive(DerivationPath.fromEncoded(path).get)
      equalBase58(derived.keyBytes, expectedKey)
    }
  }

  property("1627 BIP32 key derivation fix (31 bit child key)") {
    // see https://github.com/ergoplatform/ergo/issues/1627 for details
    val seedStr =
      "race relax argue hair sorry riot there spirit ready fetch food hedgehog hybrid mobile pretty"
    val seed: Array[Byte] = Mnemonic.toSeed(SecretString.create(seedStr))
    val path = "m/44'/429'/0'/0/0"
    val addressEncoder = ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)

    val pre1627DerivedSecretKey = ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation = true)
      .derive(DerivationPath.fromEncoded(path).get)

    P2PKAddress(pre1627DerivedSecretKey.publicKey.key)(addressEncoder).toString shouldEqual 
      "9ewv8sxJ1jfr6j3WUSbGPMTVx3TZgcJKdnjKCbJWhiJp5U62uhP"  

    val fixedDerivedSecretKey = ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation = false)
      .derive(DerivationPath.fromEncoded(path).get)

    P2PKAddress(fixedDerivedSecretKey.publicKey.key)(addressEncoder).toString shouldEqual 
      "9eYMpbGgBf42bCcnB2nG3wQdqPzpCCw5eB1YaWUUen9uCaW3wwm"  
  }

}
