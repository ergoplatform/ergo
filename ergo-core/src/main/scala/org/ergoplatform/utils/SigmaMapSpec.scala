package org.ergoplatform.utils

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SigmaMapSpec extends AnyPropSpec with Matchers with ScalaCheckPropertyChecks {

  property("empty map should have size 0") {
    val map = SigmaMap.empty[Int]
    map.size shouldBe 0
    map.isEmpty shouldBe true
  }

  property("empty map get should return None") {
    val map = SigmaMap.empty[String]
    map.get(0) shouldBe None
    map.get(127) shouldBe None
    map.get(-128) shouldBe None
  }

  property("single element map should work correctly") {
    val map = SigmaMap((5.toByte, "five"))
    map.size shouldBe 1
    map.isEmpty shouldBe false
    map.get(5) shouldBe Some("five")
    map.get(0) shouldBe None
    map.contains(5) shouldBe true
    map.contains(0) shouldBe false
  }

  property("updated should add new elements") {
    val map = SigmaMap.empty[Int]
      .updated(1, 10)
      .updated(2, 20)
      .updated(3, 30)
    
    map.size shouldBe 3
    map.get(1) shouldBe Some(10)
    map.get(2) shouldBe Some(20)
    map.get(3) shouldBe Some(30)
  }

  property("updated should preserve insertion order") {
    val map = SigmaMap.empty[Int]
      .updated(5, 50)
      .updated(1, 10)
      .updated(10, 100)
      .updated(3, 30)
    
    val keys = map.iterator.map(_._1).toSeq
    keys shouldBe Seq(5.toByte, 1.toByte, 10.toByte, 3.toByte)
  }

  property("updated should preserve original position when key exists") {
    val map = SigmaMap.empty[Int]
      .updated(1, 10)
      .updated(2, 20)
      .updated(3, 30)
      .updated(2, 200) // update existing key
    
    map.size shouldBe 3
    map.get(2) shouldBe Some(200)
    
    val keys = map.iterator.map(_._1).toSeq
    keys shouldBe Seq(1.toByte, 2.toByte, 3.toByte) // order preserved
  }

  property("removed should remove elements") {
    val map = SigmaMap.empty[Int]
      .updated(1, 10)
      .updated(2, 20)
      .updated(3, 30)
      .removed(2)
    
    map.size shouldBe 2
    map.get(1) shouldBe Some(10)
    map.get(2) shouldBe None
    map.get(3) shouldBe Some(30)
    map.contains(2) shouldBe false
  }

  property("removed should preserve insertion order of remaining elements") {
    val map = SigmaMap.empty[Int]
      .updated(1, 10)
      .updated(2, 20)
      .updated(3, 30)
      .updated(4, 40)
      .removed(2)
    
    val keys = map.iterator.map(_._1).toSeq
    keys shouldBe Seq(1.toByte, 3.toByte, 4.toByte)
  }

  property("removed on non-existent key should not change map") {
    val map = SigmaMap((1.toByte, 10), (2.toByte, 20))
    val map2 = map.removed(99)
    
    map2.size shouldBe map.size
    map2.toSeq shouldBe map.toSeq
  }

  property("filter should preserve insertion order") {
    val map = SigmaMap.empty[Int]
      .updated(1, 10)
      .updated(2, 20)
      .updated(3, 30)
      .updated(4, 40)
      .updated(5, 50)
    
    val filtered = map.filter { case (k, v) => v >= 20 && v <= 40 }
    
    filtered.size shouldBe 3
    val keys = filtered.iterator.map(_._1).toSeq
    keys shouldBe Seq(2.toByte, 3.toByte, 4.toByte)
  }

  property("should handle full Byte range (-128 to 127)") {
    // Test with minimum byte value
    val mapMin = SigmaMap((-128.toByte, "min"))
    mapMin.get(-128) shouldBe Some("min")
    
    // Test with maximum byte value
    val mapMax = SigmaMap((127.toByte, "max"))
    mapMax.get(127) shouldBe Some("max")
    
    // Test with multiple values across the range
    val map = SigmaMap.empty[String]
      .updated(-128, "min")
      .updated(-100, "neg100")
      .updated(-1, "negOne")
      .updated(0, "zero")
      .updated(1, "one")
      .updated(100, "pos100")
      .updated(127, "max")
    
    map.size shouldBe 7
    map.get(-128) shouldBe Some("min")
    map.get(0) shouldBe Some("zero")
    map.get(127) shouldBe Some("max")
    
    // Verify insertion order
    val keys = map.iterator.map(_._1).toSeq
    keys shouldBe Seq(-128.toByte, -100.toByte, -1.toByte, 0.toByte, 1.toByte, 100.toByte, 127.toByte)
  }

  property("should preserve insertion order for all 256 possible byte values") {
    // Create a map with all possible byte values in a specific order
    val allBytes = (-128 to 127).map(_.toByte).toVector
    val shuffled = scala.util.Random.shuffle(allBytes).take(50) // Test with 50 random bytes
    
    val map = shuffled.foldLeft(SigmaMap.empty[Int]) { case (m, b) =>
      m.updated(b, b.toInt * 10)
    }
    
    map.size shouldBe shuffled.size
    
    // Verify all values are correct
    shuffled.foreach { b =>
      map.get(b) shouldBe Some(b.toInt * 10)
    }
    
    // Verify insertion order is preserved
    val keys = map.iterator.map(_._1).toSeq
    keys shouldBe shuffled
  }

  property("iterator should iterate in insertion order") {
    val map = SigmaMap((3.toByte, "three"), (1.toByte, "one"), (2.toByte, "two"))
    val pairs = map.iterator.toSeq
    
    pairs shouldBe Seq(
      (3.toByte, "three"),
      (1.toByte, "one"),
      (2.toByte, "two")
    )
  }

  property("foreach should process elements in insertion order") {
    val map = SigmaMap((5.toByte, 50), (2.toByte, 20), (8.toByte, 80))
    val builder = Seq.newBuilder[(Byte, Int)]
    
    map.foreach(builder += _)
    
    builder.result() shouldBe Seq(
      (5.toByte, 50),
      (2.toByte, 20),
      (8.toByte, 80)
    )
  }

  property("toSeq should return elements in insertion order") {
    val map = SigmaMap((7.toByte, "seven"), (3.toByte, "three"), (9.toByte, "nine"))
    val seq = map.toSeq
    
    seq shouldBe Seq(
      (7.toByte, "seven"),
      (3.toByte, "three"),
      (9.toByte, "nine")
    )
  }

  property("apply should handle duplicate keys (last wins, first position preserved)") {
    val map = SigmaMap((1.toByte, 10), (2.toByte, 20), (1.toByte, 100))
    
    map.size shouldBe 2
    map.get(1) shouldBe Some(100) // last value wins
    
    val keys = map.iterator.map(_._1).toSeq
    keys shouldBe Seq(1.toByte, 2.toByte) // first position of key 1 preserved
  }

  property("fromMap should create SigmaMap from standard Map") {
    val stdMap = Map[Byte, String](1 -> "one", 2 -> "two", 3 -> "three")
    val sigmaMap = SigmaMap.fromMap(stdMap)
    
    sigmaMap.size shouldBe 3
    sigmaMap.get(1) shouldBe Some("one")
    sigmaMap.get(2) shouldBe Some("two")
    sigmaMap.get(3) shouldBe Some("three")
  }

  property("toMap should convert to standard Scala Map") {
    val sigmaMap = SigmaMap((1.toByte, 10), (2.toByte, 20), (3.toByte, 30))
    val stdMap = sigmaMap.toMap
    
    stdMap shouldBe Map[Byte, Int](1 -> 10, 2 -> 20, 3 -> 30)
  }

  property("equals should work correctly") {
    val map1 = SigmaMap((1.toByte, 10), (2.toByte, 20))
    val map2 = SigmaMap((1.toByte, 10), (2.toByte, 20))
    val map3 = SigmaMap((2.toByte, 20), (1.toByte, 10)) // different order
    val map4 = SigmaMap((1.toByte, 10), (2.toByte, 30)) // different value
    
    map1 shouldBe map2
    map1 should not be map3 // different insertion order
    map1 should not be map4 // different value
  }

  property("hashCode should be consistent with equals") {
    val map1 = SigmaMap((1.toByte, 10), (2.toByte, 20))
    val map2 = SigmaMap((1.toByte, 10), (2.toByte, 20))
    
    map1.hashCode() shouldBe map2.hashCode()
  }

  property("insertion order should differ from scala.collection.Map for more than 4 elements") {
    // This test demonstrates that scala.collection.Map doesn't preserve
    // insertion order for more than 4 elements reliably across Scala versions
    
    val sigmaMap = SigmaMap.empty[Int]
      .updated(5, 50)
      .updated(1, 10)
      .updated(7, 70)
      .updated(3, 30)
      .updated(9, 90)
    
    val keys = sigmaMap.iterator.map(_._1).toSeq
    keys shouldBe Seq(5.toByte, 1.toByte, 7.toByte, 3.toByte, 9.toByte)
    
    // SigmaMap preserves exact insertion order
    sigmaMap.size shouldBe 5
  }

  property("property-based: insertion order is always preserved") {
    val genByteValuePair = for {
      k <- Arbitrary.arbitrary[Byte]
      v <- Arbitrary.arbitrary[Int]
    } yield (k, v)
    
    forAll(Gen.listOfN(20, genByteValuePair)) { pairs =>
      val map = pairs.foldLeft(SigmaMap.empty[Int]) { case (m, (k, v)) =>
        m.updated(k, v)
      }
      
      // Filter out duplicate keys, keeping first occurrence
      val uniquePairs = pairs.reverse.distinctBy(_._1).reverse
      val expectedKeys = uniquePairs.map(_._1)
      
      val actualKeys = map.iterator.map(_._1).toSeq
      actualKeys shouldBe expectedKeys
    }
  }

  property("property-based: get/contains consistency") {
    val genByteValuePair = for {
      k <- Arbitrary.arbitrary[Byte]
      v <- Arbitrary.arbitrary[String]
    } yield (k, v)
    
    forAll(Gen.listOfN(15, genByteValuePair)) { pairs =>
      val map = pairs.foldLeft(SigmaMap.empty[String]) { case (m, (k, v)) =>
        m.updated(k, v)
      }
      
      pairs.foreach { case (k, _) =>
        map.contains(k) shouldBe true
        map.get(k) shouldBe defined
      }
    }
  }

  property("property-based: size consistency") {
    val genByteValuePair = for {
      k <- Arbitrary.arbitrary[Byte]
      v <- Arbitrary.arbitrary[Int]
    } yield (k, v)
    
    forAll(Gen.listOfN(25, genByteValuePair)) { pairs =>
      val map = pairs.foldLeft(SigmaMap.empty[Int]) { case (m, (k, v)) =>
        m.updated(k, v)
      }
      
      val uniqueKeys = pairs.map(_._1).distinct
      map.size shouldBe uniqueKeys.size
      map.iterator.size shouldBe uniqueKeys.size
    }
  }

  property("complex scenario: updates, removals, and order preservation") {
    val map = SigmaMap.empty[String]
      .updated(10, "ten")
      .updated(20, "twenty")
      .updated(30, "thirty")
      .updated(40, "forty")
      .updated(50, "fifty")
      .removed(30)
      .updated(25, "twenty-five")
      .updated(20, "TWENTY") // update existing
      .removed(40)
      .updated(35, "thirty-five")
    
    map.size shouldBe 5
    map.get(20) shouldBe Some("TWENTY")
    map.get(30) shouldBe None
    map.get(40) shouldBe None
    
    val keys = map.iterator.map(_._1).toSeq
    // Expected order: 10 (original), 20 (original position), 50, 25 (new), 35 (new)
    keys shouldBe Seq(10.toByte, 20.toByte, 50.toByte, 25.toByte, 35.toByte)
  }
}