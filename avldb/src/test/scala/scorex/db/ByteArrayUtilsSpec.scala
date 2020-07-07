package scorex.db

import com.google.common.primitives.{Ints, Shorts}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ByteArrayUtilsSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    .map(_.toArray).suchThat(_.length > 0)

  property("compare works properly") {

    //Simple and inefficient way to order byte arrays, based on
    // https://stackoverflow.com/questions/7109943/how-to-define-orderingarraybyte
    // but we compare unsigned bytes
    val ordering: Ordering[Array[Byte]] = Ordering.by((_: Array[Byte]).toIterable.map(_ & 0xFF))

    forAll(nonEmptyBytesGen, nonEmptyBytesGen) { case (bs1, bs2) =>
      val efficientOrdering = Seq(bs1, bs2).sorted(ByteArrayUtils.ByteArrayOrdering)
      val simpleOrdering = Seq(bs1, bs2).sorted(ordering)

      efficientOrdering(0).sameElements(simpleOrdering(0)) shouldBe true
      efficientOrdering(1).sameElements(simpleOrdering(1)) shouldBe true
    }
  }

  property("putInt") {
    forAll(Gen.choose(Int.MinValue, Int.MaxValue)) { i =>
      val ba = Array.fill(4)(0: Byte)
      ByteArrayUtils.putInt(ba, 0, i)
      Ints.fromByteArray(ba) shouldBe i // check with Guava
    }
  }

  property("putShort") {
    forAll(Gen.choose(Short.MinValue, Short.MaxValue)) { i =>
      val ba = Array.fill(2)(0: Byte)
      ByteArrayUtils.putShort(ba, 0, i)
      Shorts.fromByteArray(ba) shouldBe i // check with Guava
    }
  }

}
