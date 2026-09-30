package org.ergoplatform.settings

import org.ergoplatform.ErgoAddressEncoder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NetworkTypeSpec extends AnyFlatSpec with Matchers {

  "NetworkType.MainNet" should "have correct verboseName" in {
    NetworkType.MainNet.verboseName shouldBe "mainnet"
  }

  it should "be marked as mainnet" in {
    NetworkType.MainNet.isMainNet shouldBe true
    NetworkType.MainNet.isTestNet shouldBe false
  }

  it should "use mainnet address prefix" in {
    NetworkType.MainNet.addressPrefix shouldBe ErgoAddressEncoder.MainnetNetworkPrefix
  }

  "NetworkType.TestNet" should "have correct verboseName" in {
    NetworkType.TestNet.verboseName shouldBe "testnet"
  }

  it should "be marked as testnet" in {
    NetworkType.TestNet.isMainNet shouldBe false
    NetworkType.TestNet.isTestNet shouldBe true
  }

  it should "use testnet address prefix" in {
    NetworkType.TestNet.addressPrefix shouldBe ErgoAddressEncoder.TestnetNetworkPrefix
  }

  "NetworkType.Tests" should "have correct verboseName" in {
    NetworkType.Tests.verboseName shouldBe "tests"
  }

  it should "be marked as testnet" in {
    NetworkType.Tests.isMainNet shouldBe false
    NetworkType.Tests.isTestNet shouldBe true
  }

  it should "use testnet address prefix" in {
    NetworkType.Tests.addressPrefix shouldBe ErgoAddressEncoder.TestnetNetworkPrefix
  }

  "NetworkType.DevNet" should "have correct verboseName" in {
    NetworkType.DevNet.verboseName shouldBe "devnet"
  }

  it should "not be marked as mainnet or testnet" in {
    NetworkType.DevNet.isMainNet shouldBe false
    NetworkType.DevNet.isTestNet shouldBe false
  }

  it should "use devnet address prefix" in {
    NetworkType.DevNet.addressPrefix shouldBe 32
  }

  "NetworkType.DevNet60" should "have correct verboseName" in {
    NetworkType.DevNet60.verboseName shouldBe "devnet60"
  }

  it should "not be marked as mainnet or testnet" in {
    NetworkType.DevNet60.isMainNet shouldBe false
    NetworkType.DevNet60.isTestNet shouldBe false
  }

  it should "use devnet address prefix" in {
    NetworkType.DevNet60.addressPrefix shouldBe 32
  }

  "NetworkType.all" should "include main network types" in {
    NetworkType.all should contain theSameElementsAs Seq(
      NetworkType.MainNet,
      NetworkType.TestNet,
      NetworkType.DevNet
    )
  }

  it should "not include Tests (synthetic type)" in {
    NetworkType.all should not contain (NetworkType.Tests)
  }

  it should "not include DevNet60" in {
    NetworkType.all should not contain (NetworkType.DevNet60)
  }

  "NetworkType.fromString" should "recognize 'mainnet'" in {
    NetworkType.fromString("mainnet") shouldBe Some(NetworkType.MainNet)
  }

  it should "recognize 'testnet'" in {
    NetworkType.fromString("testnet") shouldBe Some(NetworkType.TestNet)
  }

  it should "recognize 'devnet'" in {
    NetworkType.fromString("devnet") shouldBe Some(NetworkType.DevNet)
  }

  it should "recognize 'devnet60'" in {
    NetworkType.fromString("devnet60") shouldBe Some(NetworkType.DevNet60)
  }

  it should "return None for invalid name" in {
    NetworkType.fromString("invalid") shouldBe None
  }

  it should "be case-sensitive" in {
    NetworkType.fromString("MainNet") shouldBe None
    NetworkType.fromString("MAINNET") shouldBe None
    NetworkType.fromString("TestNet") shouldBe None
    NetworkType.fromString("DevNet") shouldBe None
  }

  it should "return None for empty string" in {
    NetworkType.fromString("") shouldBe None
  }

  "NetworkType equality" should "work correctly for same types" in {
    NetworkType.MainNet shouldBe NetworkType.MainNet
    NetworkType.TestNet shouldBe NetworkType.TestNet
    NetworkType.Tests shouldBe NetworkType.Tests
    NetworkType.DevNet shouldBe NetworkType.DevNet
    NetworkType.DevNet60 shouldBe NetworkType.DevNet60
  }

  it should "work correctly for different types" in {
    NetworkType.MainNet should not be NetworkType.TestNet
    NetworkType.TestNet should not be NetworkType.DevNet
    NetworkType.Tests should not be NetworkType.MainNet
  }

}
