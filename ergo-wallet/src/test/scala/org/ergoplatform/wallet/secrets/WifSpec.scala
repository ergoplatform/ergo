package org.ergoplatform.wallet.secrets

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.crypto.hash.Sha256
import scorex.util.encode.Base58

import java.util

class WifSpec
  extends AnyPropSpec
    with Matchers
    with ScalaCheckPropertyChecks {

  private val scalarGen = org.scalacheck.Gen
    .containerOfN[Array, Byte](Wif.SecretLength, org.scalacheck.Arbitrary.arbByte.arbitrary)

  property("round-trips random 32-byte scalars on mainnet and testnet") {
    forAll(scalarGen) { scalar =>
      val mainnetWif = Wif.encode(scalar, mainnet = true)
      util.Arrays.equals(Wif.decode(mainnetWif).get, scalar) shouldBe true

      val testnetWif = Wif.encode(scalar, mainnet = false)
      util.Arrays.equals(Wif.decode(testnetWif).get, scalar) shouldBe true
    }
  }

  property("emits the chosen Ergo version byte") {
    forAll(scalarGen) { scalar =>
      val main = Base58.decode(Wif.encode(scalar, mainnet = true)).get
      main(0) shouldBe Wif.MainnetByte
      val test = Base58.decode(Wif.encode(scalar, mainnet = false)).get
      test(0) shouldBe Wif.TestnetByte
    }
  }

  property("emits the 0x01 compression flag") {
    forAll(scalarGen) { scalar =>
      val raw = Base58.decode(Wif.encode(scalar, mainnet = true)).get
      // last 4 bytes are checksum; byte before that is the flag
      raw(raw.length - Wif.ChecksumLength - 1) shouldBe Wif.CompressionFlag
    }
  }

  property("accepts a Bitcoin-prefixed WIF (no Ergo prefix required)") {
    // Known Bitcoin test vector: compressed mainnet WIF starting with 'K'/'L'.
    // Private key (hex): 0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d
    val knownWif = "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617"
    val expectedHex = "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
    val decoded = Wif.decode(knownWif).get
    decoded.map("%02x".format(_)).mkString shouldBe expectedHex
  }

  property("accepts both flag-present and flag-absent payloads") {
    forAll(scalarGen) { scalar =>
      val withFlag = Wif.encode(scalar, mainnet = true)
      // Construct a flag-absent WIF by hand to make sure decode tolerates it.
      val noFlag = buildWifWithoutFlag(scalar, Wif.MainnetByte)
      util.Arrays.equals(Wif.decode(withFlag).get, scalar) shouldBe true
      util.Arrays.equals(Wif.decode(noFlag).get, scalar) shouldBe true
    }
  }

  property("rejects mutated checksum") {
    forAll(scalarGen) { scalar =>
      val wif = Wif.encode(scalar, mainnet = true)
      val bytes = Base58.decode(wif).get
      bytes(bytes.length - 1) = (bytes(bytes.length - 1) ^ 0xff).toByte
      Wif.decode(Base58.encode(bytes)).isFailure shouldBe true
    }
  }

  property("rejects unknown version byte") {
    forAll(scalarGen) { scalar =>
      val bytes = Base58.decode(Wif.encode(scalar, mainnet = true)).get
      bytes(0) = 0x42.toByte
      // re-checksum so only the version byte is the problem
      val payload = bytes.dropRight(Wif.ChecksumLength)
      val newChecksum = Sha256.hash(Sha256.hash(payload)).take(Wif.ChecksumLength)
      val rebuilt = payload ++ newChecksum
      Wif.decode(Base58.encode(rebuilt)).isFailure shouldBe true
    }
  }

  property("rejects garbage input") {
    Wif.decode("not_base58!!!").isFailure shouldBe true
    Wif.decode("").isFailure shouldBe true
    // Valid Base58 but wrong length
    Wif.decode("abc").isFailure shouldBe true
  }

  private def buildWifWithoutFlag(scalar: Array[Byte], version: Byte): String = {
    val payload = version +: scalar
    val checksum = Sha256.hash(Sha256.hash(payload)).take(Wif.ChecksumLength)
    Base58.encode(payload ++ checksum)
  }

}
