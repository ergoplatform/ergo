package org.ergoplatform.wallet.secrets

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}
import scorex.util.encode.Base16

class SecretDerivationSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  it should "generate correct secret from a given seed" in {
    val cases = Table(
      ("seed_strength", "seed", "index", "secret"),
      (224, "63204534b0ae6a245acb261273740afa652fa77962e52e3e0572a32f", 1, "9363fa3260fd122a2c1785e3f8f5bf2cdf3d652e8d9d6d55bc0c1876e0c6863a"),
      (128, "83ddee9c6f656a29696152279a35b076", 1, "c8759235924d5c0c81bc4c6fc9068088e603993bf782e5ae6fae48dcf24d37ea"),
      (160, "033a0bdb0e7cc4288ec8a3875ba1af60db52c011", 1, "882c2dd713d3d4d4969c67ad19713943af69622955fbe382272ae03a49baade6"),
      (192, "01a05051c1b6fde268dab4fc7e342d864978ecf98c584f23", 1, "cac4a547b2eeffe9b3cc073a0bd4a8f8d42cc92a31f18cd8e4be91dab33733e4"),
      (256, "51b6cbc7e41ad6673962da189497e212bc8c3bb6a591ae615a8dac2e91f882f1", 1, "2641861c505d19cca19c22f588e9a36020f0655f5c470a97a2812b9b559f0221")
    )

    forAll(cases) { (_, seed, idx, secret) =>
      runCheck(seed, idx, secret)
    }
  }

  def runCheck(seed: String, index: Int, secret: String): Assertion =
    Base16.encode(secretFromSeed(index, seed)) shouldEqual secret

}
