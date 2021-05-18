package org.ergoplatform.network

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BucketingPartitionerSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  property("elements should be evenly distributed in buckets limited by bucket size") {
    forAll(Gen.nonEmptyListOf(Gen.alphaNumChar), Gen.nonEmptyListOf(Gen.alphaNumChar)) { case (buckets, elements) =>
      val (elemsType_1, elemsType_2) = elements.splitAt(elements.size/2)
      val elemsByBucket = BucketingPartitioner.distribute(buckets, Integer.MAX_VALUE, 5) { count =>
        count shouldBe buckets.size * 5
        Map("A" -> elemsType_1.take(count), "B" -> elemsType_2.take(count))
      }
      elemsByBucket shouldNot be(empty)
      elemsByBucket.forall(_._2.nonEmpty) shouldBe true
      elemsByBucket.map(_._2.size).toSet.size <= 2 shouldBe true // evenly distributed
      elemsByBucket.forall(_._2.size <= 5) shouldBe true // max sized batches
    }
  }

  property("elements should never reach max elements per type") {
    forAll(Gen.nonEmptyListOf(Gen.alphaNumChar), Gen.nonEmptyListOf(Gen.alphaNumChar)) { case (buckets, elements) =>
      val (elemsType_1, elemsType_2) = elements.splitAt(elements.size/2)
      val elemsByBucket = BucketingPartitioner.distribute(buckets, 5, 100) { count =>
        count shouldBe 5
        Map("A" -> elemsType_1.take(count), "B" -> elemsType_2.take(count))
      }
      elemsByBucket.map(_._2.size).sum <= 2*5 shouldBe true // max size of elements
    }
  }

  property("empty buckets or elements cannot be partitioned") {
    BucketingPartitioner.distribute(List.empty, Integer.MAX_VALUE, 5)(_ => Map("A" -> List(1))).size shouldBe 0
    BucketingPartitioner.distribute(List(1), Integer.MAX_VALUE, 5)(_ => Map.empty[String, Seq[Int]]).size shouldBe 0
    BucketingPartitioner.distribute(List.empty, Integer.MAX_VALUE, 5)(_ => Map.empty[String, Seq[Int]]).size shouldBe 0
  }

  property("0 or negative count of elements to fetch cannot be partitioned") {
    BucketingPartitioner.distribute(List.empty, -1, 5)(_ => Map("A" -> List(1))).size shouldBe 0
    BucketingPartitioner.distribute(List.empty, 5, 0)(_ => Map("A" -> List(1))).size shouldBe 0
  }

  property("less or equal elements than buckets should return one element per bucket") {
    BucketingPartitioner.distribute(List(1,2,3), Integer.MAX_VALUE, 5) { _ =>
      Map("A" -> List(1))
    } shouldBe Map((1, "A") -> List(1))
  }

  property("elements should be distributed into bucket-types") {
    BucketingPartitioner.distribute(List(1,2), Integer.MAX_VALUE, 1) { _ =>
      Map("A" -> List(1,2), "B" -> List(1,2), "C" -> List(1,2))
    } shouldBe Map((2,"B") -> List(2), (1,"C") -> List(1), (1,"B") -> List(1), (1,"A") -> List(1), (2,"C") -> List(2), (2,"A") -> List(2))
  }

}
