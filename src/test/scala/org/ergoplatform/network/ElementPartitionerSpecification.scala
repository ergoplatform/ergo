package org.ergoplatform.network

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ElementPartitionerSpecification
  extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers {

  property("elements should be evenly distributed in buckets limited by bucket size") {
    forAll(Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.toSet), Gen.nonEmptyListOf(Gen.alphaNumChar)) {
      case (buckets, elements) =>
        val (elemsType_1, elemsType_2) = elements.splitAt(elements.size / 2)
        val elemsByBucket =
          ElementPartitioner.distribute(buckets, Integer.MAX_VALUE, 1, 5) { count =>
            count shouldBe buckets.size * 5
            Map("A" -> elemsType_1.take(count), "B" -> elemsType_2.take(count))
          }
        elemsByBucket shouldNot be(empty)
        elemsByBucket.forall(_._2.nonEmpty) shouldBe true
        elemsByBucket.map(_._2.size).toSet.size <= 2 shouldBe true // evenly distributed
        elemsByBucket.forall(_._2.size <= 5) shouldBe true // max sized batches
    }
  }

  property("elements should never reach max elements") {
    forAll(Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.toSet), Gen.nonEmptyListOf(Gen.alphaNumChar)) {
      case (buckets, elements) =>
        val (elemsType_1, elemsType_2) = elements.splitAt(elements.size / 2)
        val elemsByBucket = ElementPartitioner.distribute(buckets, 5, 1, 100) { count =>
          count shouldBe 5
          Map("A" -> elemsType_1.take(count), "B" -> elemsType_2.take(count))
        }
        elemsByBucket.map(_._2.size).sum <= 2 * 5 shouldBe true // max size of elements
    }
  }

  property(
    "bucket should contain less elements than minElementsPerBucket only if not enough elements is available"
  ) {
    forAll(Gen.listOf(Gen.alphaNumChar).map(_.toSet), Gen.listOf(Gen.alphaNumChar).map(_.toSet)) {
      case (buckets, elements) =>
        val elemsByBucket =
          ElementPartitioner.distribute(buckets, Integer.MAX_VALUE, 5, 10) { count =>
            assert(count <= 10 * buckets.size)
            Map("A" -> elements.take(count).toSeq)
          }
        // if there is only 4 elements available to download, then bucket may contain only 4 elements regardless of minElementsPerBucket=5
        if (elements.size >= 5) {
          elemsByBucket.map(_._2.size).foreach { count =>
            assert(count >= 5 && count <= 10)
          }
        }
    }
  }

  property("empty buckets or elements cannot be partitioned") {
    ElementPartitioner
      .distribute(Set.empty, Integer.MAX_VALUE, 1, 5)(_ => Map("A" -> List(1)))
      .size shouldBe 0
    ElementPartitioner
      .distribute(Set(1), Integer.MAX_VALUE, 1, 5)(_ => Map.empty[String, List[Int]])
      .size shouldBe 0
    ElementPartitioner
      .distribute(Set.empty, Integer.MAX_VALUE, 1, 5)(_ => Map.empty[String, List[Int]])
      .size shouldBe 0
  }

  property("0 or negative count of elements to fetch cannot be partitioned") {
    ElementPartitioner
      .distribute(Set.empty, -1, 1, 5)(_ => Map("A" -> List(1)))
      .size shouldBe 0
    ElementPartitioner
      .distribute(Set.empty, 5, 1, 0)(_ => Map("A" -> List(1)))
      .size shouldBe 0
  }

  property("less or equal elements than buckets should return one element per bucket") {
    ElementPartitioner.distribute(Set(1, 2, 3), Integer.MAX_VALUE, 1, 5) { _ =>
      Map("A" -> List(1))
    } shouldBe Map((1, "A") -> Set(1))
  }

  property("elements should be distributed into bucket-types") {
    ElementPartitioner.distribute(Set(1, 2), Integer.MAX_VALUE, 1, 1) { _ =>
      Map("A" -> List(1, 2), "B" -> List(1, 2), "C" -> List(1, 2))
    } shouldBe Map(
      (2, "B") -> Set(2),
      (1, "C") -> Set(1),
      (1, "B") -> Set(1),
      (1, "A") -> Set(1),
      (2, "C") -> Set(2),
      (2, "A") -> Set(2)
    )
  }

  property(
    "minElementsPerBucket constraint should not be used if there is less elements available"
  ) {
    val elems =
      ElementPartitioner.distribute(Set(1), Integer.MAX_VALUE, 100, 1) { _ =>
        Map("A" -> List(1))
      }
    elems.size shouldBe 1
    elems.head._2.size shouldBe 1
  }
}
