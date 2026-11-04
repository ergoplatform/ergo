package org.ergoplatform.db

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class LDBKVStoreSpec extends AnyPropSpec with Matchers with DBSpec {

  property("put/get/getAll/delete") {
    withStore { store =>
      val valueA = (byteString("A"), byteString("1"))
      val valueB = (byteString("B"), byteString("2"))

      store.update(Array(valueA._1, valueB._1), Array(valueA._2, valueB._2), toRemove = Array.empty).get

      store.get(valueA._1).toBs shouldBe Some(valueA._2).toBs
      store.get(valueB._1).toBs shouldBe Some(valueB._2).toBs

      store.getAll.toSeq.toBs shouldBe Seq(valueA, valueB).toBs

      store.update(Array.empty, Array.empty, toRemove = Array(valueA._1)).get
      store.get(valueA._1) shouldBe None
    }
  }

  property("record rewriting") {
    withStore { store =>
      val key = byteString("A")
      val valA = byteString("1")
      val valB = byteString("2")

      store.insert(key, valA).get

      store.get(key).toBs shouldBe Some(valA).toBs

      store.insert(key, valB).get

      store.get(key).toBs shouldBe Some(valB).toBs

      store.getAll.size shouldBe 1
    }
  }

  /**
    * Unsigned lexicographic sort key for a raw store key, matching the store's byte
    * comparator. Rendered as hex so that plain string ordering equals byte ordering.
    */
  private def keyOrder(k: Array[Byte]): String =
    k.map(b => f"${b & 0xff}%02x").mkString

  /** Sort key for a key-value entry, by its key. */
  private def entryOrder(e: (Array[Byte], Array[Byte])): String = keyOrder(e._1)

  /** Hex-encoded entry whose key is at or after `from`, in store order. */
  private def from(store: scorex.db.LDBKVStore,
                   fromKey: String,
                   limit: Int): Seq[String] =
    store.scanFrom(byteString(fromKey), limit, _ => true, _ => true)
      .map(e => s"${keyOrder(e._1)}->${keyOrder(e._2)}")

  /** Insert `entries` (in arbitrary order) and return them in store key order. */
  private def seed(store: scorex.db.LDBKVStore,
                   entries: Seq[(Array[Byte], Array[Byte])]): Seq[String] = {
    store.update(entries.map(_._1).toArray, entries.map(_._2).toArray, Array.empty).get
    entries.sortBy(entryOrder).map(e => s"${keyOrder(e._1)}->${keyOrder(e._2)}")
  }

  property("scanFrom collects matching keys in ascending order") {
    withStore { store =>
      val entries = (1 to 5).map(i => byteString(f"A$i") -> byteString(s"v$i"))
      val sorted = seed(store, entries)
      from(store, "A", Int.MaxValue) shouldBe sorted
    }
  }

  property("scanFrom starts at the given key, inclusive") {
    withStore { store =>
      val entries = (1 to 5).map(i => byteString(f"A$i") -> byteString(s"v$i"))
      val sorted = seed(store, entries)
      // seek is inclusive, so A3 itself must be in the result
      val fromA3 = from(store, "A3", Int.MaxValue)
      fromA3 shouldBe sorted.drop(2)
      fromA3 should contain("4133->7633") // hex("A3")->hex("v3")
    }
  }

  property("scanFrom honours the limit") {
    withStore { store =>
      val entries = (1 to 10).map(i => byteString(f"A$i%02d") -> byteString(s"v$i"))
      val sorted = seed(store, entries)
      from(store, "A", 4) shouldBe sorted.take(4)
      // a zero or negative limit must not scan at all
      from(store, "A", 0) shouldBe empty
    }
  }

  property("scanFrom applies keyFilter and counts only matches against the limit") {
    withStore { store =>
      // even keys accepted, odd skipped; zero-padded so order is A01..A10
      val entries = (1 to 10).map(i => byteString(f"A$i%02d") -> byteString(s"v$i"))
      seed(store, entries)

      val isEven = (k: Array[Byte]) => (k(k.length - 1) - '0') % 2 == 0
      val scan = (limit: Int) => store.scanFrom(byteString("A"), limit, isEven, _ => true)
        .map(e => s"${keyOrder(e._1)}->${keyOrder(e._2)}")

      // ground truth: the even-suffixed keys (A02, A04, A06, A08, A10) in store order
      val evens = entries.filter(e => isEven(e._1)).sortBy(entryOrder)
        .map(e => s"${keyOrder(e._1)}->${keyOrder(e._2)}")
      evens should have size 5
      // the limit bounds accepted entries, not scanned ones: 3 gives the first 3 even
      // keys (A02, A04, A06) even though 6 keys must be read to find them
      scan(3) shouldBe evens.take(3)
      scan(1) shouldBe evens.take(1)
      // asking for more even keys than exist returns just those that exist
      scan(99) shouldBe evens
    }
  }

  property("scanFrom stops early when continueScan fails") {
    withStore { store =>
      // single-digit suffixes only, so the last byte identifies the key unambiguously
      val entries = (1 to 9).map(i => byteString(f"A$i") -> byteString(s"v$i"))
      val sorted = seed(store, entries)

      // stop as soon as A5 is reached, so only A1..A4 can be returned
      val below5 = (k: Array[Byte]) => k(k.length - 1) - '0' < 5
      val res = store.scanFrom(byteString("A"), Int.MaxValue, _ => true, below5)
        .map(e => s"${keyOrder(e._1)}->${keyOrder(e._2)}")
      res shouldBe sorted.take(4)
    }
  }

  property("scanFrom on an empty store and past the last key") {
    withStore { store =>
      from(store, "A", 10) shouldBe empty

      val entries = (1 to 3).map(i => byteString(f"A$i") -> byteString(s"v$i"))
      seed(store, entries)
      // seek beyond every key: the iterator has nothing left
      from(store, "Z", 10) shouldBe empty
    }
  }

  property("scanFrom leaves the store usable for a subsequent scan") {
    // the iterator must be closed even when a scan stops early, otherwise the db breaks
    withStore { store =>
      val entries = (1 to 6).map(i => byteString(f"A$i") -> byteString(s"v$i"))
      val sorted = seed(store, entries)

      val below3 = (k: Array[Byte]) => k(k.length - 1) - '0' < 3
      store.scanFrom(byteString("A"), Int.MaxValue, _ => true, below3).length shouldBe 2
      // a full scan afterwards must still work and see every entry
      from(store, "A", Int.MaxValue) shouldBe sorted
    }
  }

  property("last key in range") {
    withStore { store =>
      val valueA = (byteString("A"), byteString("1"))
      val valueB = (byteString("B"), byteString("2"))
      val valueC = (byteString("C"), byteString("1"))
      val valueD = (byteString("D"), byteString("2"))
      val valueE = (byteString("E"), byteString("3"))
      val valueF = (byteString("F"), byteString("4"))

      val values = Array(valueA, valueB, valueC, valueD, valueE, valueF)
      store.insert(values.map(_._1), values.map(_._2)).get

      store.lastKeyInRange(valueA._1, valueC._1).get.toSeq shouldBe valueC._1.toSeq
      store.lastKeyInRange(valueD._1, valueF._1).get.toSeq shouldBe valueF._1.toSeq
      store.lastKeyInRange(valueF._1, byteString32("Z")).get.toSeq shouldBe valueF._1.toSeq
      store.lastKeyInRange(Array(10: Byte), valueA._1).get.toSeq shouldBe valueA._1.toSeq

      store.lastKeyInRange(Array(10: Byte), Array(11: Byte)).isDefined shouldBe false
    }
  }

}
