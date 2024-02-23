package scorex.testkit

import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.ergoplatform.serialization.ErgoSerializer

trait SerializationTests extends ScalaCheckPropertyChecks with Matchers {
  def checkSerializationRoundtrip[A](generator: Gen[A], serializer: ErgoSerializer[A]): Assertion = {
    forAll(generator) { b: A =>
      val recovered = serializer.parseBytes(serializer.toBytes(b))
      serializer.toBytes(b) shouldEqual serializer.toBytes(recovered)
    }
  }
}
