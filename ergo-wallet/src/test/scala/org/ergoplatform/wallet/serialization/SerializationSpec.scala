package org.ergoplatform.wallet.serialization

import org.ergoplatform.wallet.boxes.TrackedBoxSerializer
import org.ergoplatform.wallet.secrets.{DerivationPathSerializer, ExtendedSecretKeySerializer}
import org.ergoplatform.wallet.utils.Generators
import org.scalacheck.Gen
import org.scalatest.{Assertion, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SerializationSpec
  extends PropSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with Generators {

  def checkSerializationRoundtrip[A](generator: Gen[A],
                                     serializer: ErgoWalletSerializer[A]): Assertion = {
    forAll(generator) { b: A =>
      val recovered = serializer.parseBytes(serializer.toBytes(b))
      val bytes = serializer.toBytes(b)
      bytes shouldEqual serializer.toBytes(recovered)
      b shouldEqual recovered
    }
  }

  property("DerivationPath serialization") {
    checkSerializationRoundtrip(derivationPathGen, DerivationPathSerializer)
  }

  property("TrackedBox serialization") {
    checkSerializationRoundtrip(trackedBoxGen, TrackedBoxSerializer)
  }

  property("ExtendedSecretKey serialization") {
    checkSerializationRoundtrip(extendedSecretGen, ExtendedSecretKeySerializer)
  }

}
