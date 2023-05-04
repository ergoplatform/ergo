package org.ergoplatform.wallet.crypto

import org.ergoplatform.wallet.utils.Generators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.util.Random
import sigmastate.basics.DLogProtocol.DLogProverInput

class ErgoSignatureSpec extends AnyPropSpec with Matchers with Generators {

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

}
