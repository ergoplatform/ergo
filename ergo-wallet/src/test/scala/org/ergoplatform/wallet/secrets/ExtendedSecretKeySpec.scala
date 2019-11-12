package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}
import scorex.util.encode.Base58

class ExtendedSecretKeySpec
  extends PropSpec
    with Matchers
    with TableDrivenPropertyChecks {

  val seedStr = "edge talent poet tortoise trumpet dose"
  val seed: Array[Byte] = Mnemonic.toSeed(seedStr)

  property("key tree derivation from seed (test vectors from BIP32 check)") {
    val expectedRoot = "4rEDKLd17LX4xNR8ss4ithdqFRc3iFnTiTtQbanWJbCT"
    val cases = Seq(
      ("CLdMMHxNtiPzDnWrVuZQr22VyUx8deUG7vMqMNW7as7M", 1),
      ("9icjp3TuTpRaTn6JK6AHw2nVJQaUnwmkXVdBdQSS98xD", 2),
      ("DWMp3L9JZiywxSb5gSjc5dYxPwEZ6KkmasNiHD6VRcpJ", Index.hardIndex(2))
    )

    val root = ExtendedSecretKey.deriveMasterKey(seed)

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

    val root = ExtendedSecretKey.deriveMasterKey(seed)

    cases.foreach { case (expectedKey, path) =>
      val derived = root.derive(DerivationPath.fromEncoded(path).get)
      equalBase58(derived.keyBytes, expectedKey)
    }
  }

  def equalBase58(v1: Array[Byte], v2b58: String): Assertion = Base58.encode(v1) shouldEqual v2b58

}
