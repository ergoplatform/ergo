package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Random

class SnapshotsDbSpecification extends ErgoCorePropertyTest {
    import org.ergoplatform.utils.generators.CoreObjectGenerators._
    import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  def seededDatabase(manifestIds: Seq[ModifierId]): (SnapshotsInfo, SnapshotsDb) = {
    val m = manifestIds.map { mid =>
      Random.nextInt(1000000) -> (Digest32 @@ idToBytes(mid))
    }.toMap
    val si = new SnapshotsInfo(m)
    val dir = createTempDir.getAbsolutePath
    val db = SnapshotsDb.create(dir)
    db.writeSnapshotsInfo(si)
    si -> db
  }

  property("snapshotsInfo round-trip") {
    forAll(Gen.nonEmptyListOf(modifierIdGen)) { manifestIds =>
      val (si, db) = seededDatabase(manifestIds)
      val read = db.readSnapshotsInfo.availableManifests.mapValues(bs => bytesToId(bs))
      val siTocompare = si.availableManifests.mapValues(bs => bytesToId(bs))
      read shouldBe siTocompare
    }
  }

  property("pruneSnapshots choosing snapshots correctly") {
    forAll(Gen.nonEmptyListOf(modifierIdGen)) { manifestIds =>
      val (si, db) = seededDatabase(manifestIds)

      val toStore = Random.nextInt(manifestIds.size + 3)

      db.pruneSnapshots(toStore)

      val after = db.readSnapshotsInfo

      if (toStore >= manifestIds.size) {
        after.availableManifests.size == manifestIds.size
      } else {
        val storedKeys = si.availableManifests.keySet.toSeq.sorted.takeRight(toStore)
        val stored = si.availableManifests.filterKeys(h => storedKeys.contains(h))
        after.availableManifests.mapValues(bytesToId) shouldBe stored.mapValues(bytesToId)
      }
    }
  }

  property("pruneSnapshots preserves subtrees shared with any retained snapshot") {
    import org.ergoplatform.serialization.ManifestSerializer
    import scorex.crypto.authds.{ADKey, ADValue, Balance}
    import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes}
    import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, ProxyInternalNode}
    import scorex.crypto.hash.Blake2b256
    import scorex.db.LDBFactory

    def manifestWithSubtrees(leftSubtreeId: Digest32, rightSubtreeId: Digest32): BatchAVLProverManifest[Digest32] = {
      val zeroKey = ADKey @@ Array.fill[Byte](32)(0: Byte)
      val leafVal = ADValue @@ Array[Byte](0)
      val balance = Balance @@ (0: Byte)
      val leaf = new ProverLeaf[Digest32](zeroKey, leafVal, zeroKey)(Blake2b256)
      val bottomProxy: ProverNodes[Digest32] =
        new ProxyInternalNode[Digest32](zeroKey, leftSubtreeId, rightSubtreeId, balance)(Blake2b256)
      val root = (1 until ManifestSerializer.MainnetManifestDepth.toInt).foldLeft(bottomProxy) { (child, _) =>
        new InternalProverNode[Digest32](zeroKey, child, leaf, balance)(Blake2b256)
      }
      BatchAVLProverManifest[Digest32](root, rootHeight = ManifestSerializer.MainnetManifestDepth.toInt)
    }

    val dir = createTempDir.getAbsolutePath
    val store = LDBFactory.createKvDb(dir)
    try {
      val db = new SnapshotsDb(store)

      val sharedSubtreeId = Blake2b256("shared-between-old-and-mid")
      val oldOnlySubtreeId = Blake2b256("old-only-subtree")
      val midOnlySubtreeId = Blake2b256("mid-only-subtree")
      val latestOnlySubtreeId = Blake2b256("latest-only-subtree")

      val mOld = manifestWithSubtrees(sharedSubtreeId, oldOnlySubtreeId)
      val mMid = manifestWithSubtrees(sharedSubtreeId, midOnlySubtreeId)
      val mLatest = manifestWithSubtrees(latestOnlySubtreeId, latestOnlySubtreeId)

      val idOld = Blake2b256("manifest-old")
      val idMid = Blake2b256("manifest-mid")
      val idLatest = Blake2b256("manifest-latest")

      val ser = ManifestSerializer.defaultSerializer
      store.insert(idOld, ser.toBytes(mOld))
      store.insert(idMid, ser.toBytes(mMid))
      store.insert(idLatest, ser.toBytes(mLatest))
      store.insert(sharedSubtreeId, Array[Byte](1, 2, 3))
      store.insert(oldOnlySubtreeId, Array[Byte](4, 5, 6))
      store.insert(midOnlySubtreeId, Array[Byte](10, 11, 12))
      store.insert(latestOnlySubtreeId, Array[Byte](7, 8, 9))

      db.writeSnapshotsInfo(new SnapshotsInfo(Map(100 -> idOld, 200 -> idMid, 300 -> idLatest)))

      // Keep 2 most recent snapshots (200 and 300); prune 100
      db.pruneSnapshots(toStore = 2)

      db.readSnapshotsInfo.availableManifests.keySet shouldBe Set(200, 300)
      db.readManifestBytes(idOld) shouldBe None
      db.readSubtreeBytes(oldOnlySubtreeId) shouldBe None
      // Must still be retained because snapshot 200 references it even though snapshot 300 does not
      db.readSubtreeBytes(sharedSubtreeId) shouldBe defined
      db.readSubtreeBytes(midOnlySubtreeId) shouldBe defined
      db.readSubtreeBytes(latestOnlySubtreeId) shouldBe defined
    } finally {
      store.close()
    }
  }
}
