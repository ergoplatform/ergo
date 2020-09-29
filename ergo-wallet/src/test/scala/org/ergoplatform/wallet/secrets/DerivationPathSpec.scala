package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.Constants
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.utils.Generators
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DerivationPathSpec
  extends PropSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with Generators {

  property("derivation from encoded path") {
    forAll(derivationPathGen) { path =>
      val decodeTry = DerivationPath.fromEncoded(path.encoded)

      decodeTry shouldBe 'success
      decodeTry.get shouldEqual path
    }
  }

  property("nextPath - new default derivation") {
    implicit val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)

    // This testing pair is checked against CoinBarn and Yoroi
    val mnemonic = "liar exercise solve delay betray sheriff method empower disease river recall vacuum"
    val address = "9hAymcGaRfTX7bMADNdfWfk7CKzi2ZpvRBCmtEf6d92n8E26Ax7"

    val seed = Mnemonic.toSeed(mnemonic, None)

    val masterKey = ExtendedSecretKey.deriveMasterKey(seed)
    val dp = DerivationPath.nextPath(IndexedSeq(masterKey)).get
    val sk = masterKey.derive(dp).asInstanceOf[ExtendedSecretKey]
    val pk = sk.publicKey.key

    // The first derived key corresponds to m/44'/429'/0'/0/0 and the same as the key in CoinBarn
    // (and the first key in Yoroi)
    P2PKAddress(pk).toString() shouldBe address

    val pk2 = masterKey.derive(DerivationPath.fromEncoded("m/44'/429'/0'/0/0").get).asInstanceOf[ExtendedSecretKey].publicKey.key
    P2PKAddress(pk2).toString() shouldBe address

    // next path should be m/44'/429'/0'/0/1
    val dp2 = DerivationPath.nextPath(IndexedSeq(masterKey, sk)).get
    dp2 shouldBe DerivationPath.fromEncoded("m/44'/429'/0'/0/1").get

    // on top of old paths, derivation works as before EIP, m/1 -> m/2
    val sk2 = masterKey.derive(DerivationPath.fromEncoded("m/1").get).asInstanceOf[ExtendedSecretKey]
    val dp3 = DerivationPath.nextPath(IndexedSeq(masterKey, sk2)).get
    dp3 shouldBe DerivationPath.fromEncoded("m/2").get

    // for (m/1, m/44'/429'/0'/0/0), next path would be m/44'/429'/0'/0/1
    val dp4 = DerivationPath.nextPath(IndexedSeq(masterKey, sk2, sk)).get
    dp4 shouldBe DerivationPath.fromEncoded("m/44'/429'/0'/0/1").get
  }


  property("nextPath - old derivation") {
    implicit val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)

    // pre-EIP3 derivation testing pair got from the node
    val mnemonic = "liar exercise solve delay betray sheriff method empower disease river recall vacuum"
    val address = "9h7f11AC9RMHkhFbXg46XfYHq3HNnb1A9UtMmMYo6hAuQzWxVWu"

    val seed = Mnemonic.toSeed(mnemonic, None)

    val masterKey = ExtendedSecretKey.deriveMasterKey(seed)
    val dp = DerivationPath.nextPath(IndexedSeq(masterKey), oldDerivation = true).get
    val sk = masterKey.derive(dp).asInstanceOf[ExtendedSecretKey]
    val pk = sk.publicKey.key
    P2PKAddress(pk).toString() shouldBe address

    val dp2 = DerivationPath.nextPath(IndexedSeq(masterKey, sk)).get
    dp2 shouldBe DerivationPath.fromEncoded("m/2").get

    val sk2 = masterKey.derive(DerivationPath.fromEncoded("m/1/1").get).asInstanceOf[ExtendedSecretKey]

    val dp3 = DerivationPath.nextPath(IndexedSeq(masterKey, sk, sk2)).get
    dp3 shouldBe DerivationPath.fromEncoded("m/2").get

    val sk3 = masterKey.derive(DerivationPath.fromEncoded("m/44'/429'/0'/0/0").get).asInstanceOf[ExtendedSecretKey]
    val dp4 = DerivationPath.nextPath(IndexedSeq(masterKey, sk, sk3)).get
    dp4 shouldBe DerivationPath.fromEncoded("m/44'/429'/0'/0/1").get
  }

  property("equality of old derivation") {
    // Check that hardcoded path from old codebase corresponds to the new string form (Constants.oldDerivation)
    DerivationPath(Array(0, 1), publicBranch = false) shouldBe Constants.oldDerivation
  }

  property("master key derivation") {
    implicit val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
    val masterKeyDerivation: DerivationPath = DerivationPath.fromEncoded("m/").get

    val mnemonic = "liar exercise solve delay betray sheriff method empower disease river recall vacuum"
    val address = "9hXkYAHd1hWDroNMA3w9t6st2QyS3aTVe5w6GwWPKK5q4SmpUDL"

    val seed = Mnemonic.toSeed(mnemonic, None)

    val masterKey = ExtendedSecretKey.deriveMasterKey(seed)
    P2PKAddress(masterKey.publicKey.key).toString() shouldBe address

    masterKey.path shouldBe masterKeyDerivation

    masterKeyDerivation.isMaster shouldBe true

    val sk = masterKey.derive(masterKeyDerivation).asInstanceOf[ExtendedSecretKey]
    val pk = sk.publicKey.key
    sk.path.isMaster shouldBe true
    P2PKAddress(pk).toString() shouldBe address
  }

  property("isEip3 correctly distinguishing") {
    Constants.eip3DerivationPath.isEip3 shouldBe true
    Constants.eip3DerivationPath.toPublicBranch.isEip3 shouldBe true
    DerivationPath.fromEncoded("m/44'/429'/0'/0/1").get.isEip3 shouldBe true
    DerivationPath.fromEncoded("M/44'/429'/0'/0/1").get.isEip3 shouldBe true
    DerivationPath.fromEncoded("m/44'/429'/0'/1/1").get.isEip3 shouldBe true
    Constants.oldDerivation.isEip3 shouldBe false
    DerivationPath.fromEncoded("m/44'/429'/1'/0/1").get.isEip3 shouldBe false
  }

}
