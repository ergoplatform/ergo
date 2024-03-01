package org.ergoplatform.wallet.serialization

import org.ergoplatform.sdk.wallet.secrets.{DerivationPathSerializer, ExtendedPublicKeySerializer, ExtendedSecretKeySerializer}
import org.ergoplatform.wallet.boxes.TrackedBoxSerializer
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SerializationSpec
  extends AnyPropSpec
    with Matchers
    with ScalaCheckPropertyChecks {
  import org.ergoplatform.wallet.utils.WalletGenerators._

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
    checkSerializationRoundtrip(derivationPathGen, ErgoWalletSerializer.fromSigmaSerializer(DerivationPathSerializer))
  }

  property("TrackedBox serialization") {
    checkSerializationRoundtrip(trackedBoxGen, TrackedBoxSerializer)
  }

  property("ExtendedSecretKey serialization") {
    checkSerializationRoundtrip(extendedSecretGen, ErgoWalletSerializer.fromSigmaSerializer(ExtendedSecretKeySerializer))
  }

  property("ExtendedPublicKey serialization") {
    checkSerializationRoundtrip(extendedPubKeyGen, ErgoWalletSerializer.fromSigmaSerializer(ExtendedPublicKeySerializer))
  }

}
