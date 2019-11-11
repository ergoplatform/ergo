package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.utils.Generators
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DerivationPathSpec
  extends PropSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with Generators {

  property("derivation from encoded path") {
    forAll(derivationPathGen) { path =>
      val decodeTry = DerivationPath.fromEncoded(path.encoded)

      decodeTry shouldBe 'success
      decodeTry.get shouldEqual path
    }
  }

}
