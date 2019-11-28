package org.ergoplatform.wallet.secrets

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

}
