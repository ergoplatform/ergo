package org.ergoplatform.wallet.crypto

import org.bouncycastle.util.BigIntegers
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.hash.Blake2b256
import scorex.util.Random
import scorex.util.encode.Base16
import sigma.crypto.CryptoConstants.{dlogGroup, groupOrder}
import sigma.serialization.GroupElementSerializer
import sigmastate.crypto.DLogProtocol.DLogProverInput

class ErgoSignatureSpec extends AnyPropSpec with Matchers {
  import org.ergoplatform.wallet.utils.WalletGenerators._
  import org.ergoplatform.wallet.crypto.ErgoSignature._

  property("sign/verify") {
    val secret = DLogProverInput(genSecret.bigInteger)
    val pk = secret.publicImage

    val msg = Random.randomBytes(128)

    val sig = sign(msg, secret.w)

    verify(msg, sig, pk.value) shouldBe true
  }

  property("always produce signature of fixed length") {
    (1 to 2000).foreach { _ =>
      val sk = DLogProverInput(genSecret.bigInteger)
      val sig = sign(Random.randomBytes(128), sk.w)
      sig.length shouldBe 56
    }
  }

  property("reject non-56-byte signatures (padded, truncated, or empty)") {
    val secret = DLogProverInput(genSecret.bigInteger)
    val pk = secret.publicImage.value
    val msg = Random.randomBytes(64)
    val sig = sign(msg, secret.w)

    verify(msg, sig, pk) shouldBe true
    verify(msg, sig.take(24) ++ Array[Byte](1, 2, 3) ++ sig.takeRight(32), pk) shouldBe false
    verify(msg, sig.take(55), pk) shouldBe false
    verify(msg, Array.emptyByteArray, pk) shouldBe false
  }

  property("reject identity public key and non-canonical z >= groupOrder") {
    val msg = Random.randomBytes(64)
    val identityPk = dlogGroup.identity
    val w = dlogGroup.generator
    val prefix = Base16.decode("010027100108cd").get
    val postfix = Base16.decode("73000021").get
    val commitment = prefix ++ GroupElementSerializer.toBytes(identityPk) ++ postfix ++ GroupElementSerializer.toBytes(w)
    val cBytes = Blake2b256.hash(commitment ++ msg).take(24)

    val zOne = BigIntegers.asUnsignedByteArray(32, BigInt(1).bigInteger)
    val zOverflow = BigIntegers.asUnsignedByteArray(32, (BigInt(1) + groupOrder).bigInteger)
    verify(msg, cBytes ++ zOne, identityPk) shouldBe false
    verify(msg, cBytes ++ zOverflow, identityPk) shouldBe false

    val secret = DLogProverInput(genSecret.bigInteger)
    val pk = secret.publicImage.value
    val sig = sign(msg, secret.w)
    val zEqOrder = BigIntegers.asUnsignedByteArray(32, groupOrder)
    verify(msg, sig.take(24) ++ zEqOrder, pk) shouldBe false
  }

  property("reject null inputs in Scala and Java verify") {
    val secret = DLogProverInput(genSecret.bigInteger)
    val pk = secret.publicImage.value
    val msg = Random.randomBytes(64)
    val sig = sign(msg, secret.w)
    val javaSig = new org.ergoplatform.wallet.interface4j.crypto.ErgoSignature()

    verify(null, sig, pk) shouldBe false
    verify(msg, null, pk) shouldBe false
    verify(msg, sig, null) shouldBe false
    javaSig.verify(msg, sig, null) shouldBe false
    javaSig.verify(null, sig, null) shouldBe false
    javaSig.verify(msg, null, null) shouldBe false
  }

}
