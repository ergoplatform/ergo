package org.ergoplatform.nodeView.state

import com.google.common.primitives.Ints
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.Insert
import scorex.crypto.authds.avltree.batch.helpers.TestHelper
import scorex.crypto.hash.Digest32
import scorex.db.LDBFactory
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Random

class SnapshotsDbSpecification extends ErgoCorePropertyTest with TestHelper {
  override protected val KL: Int = 32
  override protected val VL: Int = 8

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

  property("pruneSnapshots preserves metadata when retained manifests are unavailable") {
    forAll(Gen.nonEmptyListOf(modifierIdGen)) { manifestIds =>
      val (si, db) = seededDatabase(manifestIds)

      val toStore = Random.nextInt(manifestIds.size + 3)

      db.pruneSnapshots(toStore)

      val after = db.readSnapshotsInfo

      if (toStore == 0) {
        after.availableManifests shouldBe empty
      } else {
        after.availableManifests.mapValues(bytesToId) shouldBe si.availableManifests.mapValues(bytesToId)
      }
    }
  }

  property("pruneSnapshots retains the greatest heights with readable manifests") {
    val inputs = for {
      count <- Gen.choose(5, 12)
      heights <- Gen.pick(count, 1 to 10000)
      toStore <- Gen.choose(0, count + 2)
    } yield (heights.reverse, toStore)

    forAll(inputs) { case (heights, toStore) =>
      val source = createVersionedStore()
      val destination = LDBFactory.createKvDb(createTempDir.getAbsolutePath)
      try {
        val prover = createPersistentProver(createVersionedStorage(source))
        val db = new SnapshotsDb(destination)
        heights.foreach { height =>
          val key = ADKey @@ (Array.fill(28)(0: Byte) ++ Ints.toByteArray(height))
          prover.performOneOperation(Insert(key, ADValue @@ Array.fill(8)(1: Byte))).get
          prover.generateProofAndUpdateStorage()
          db.writeSnapshot(prover.storage.asInstanceOf[STORAGE], height,
            prover.digest.dropRight(1)).get
        }
        val before = db.readSnapshotsInfo.availableManifests
        before.size shouldBe heights.size
        before.values.foreach { id =>
          ManifestSerializer.defaultSerializer.parseBytesTry(db.readManifestBytes(id).get).isSuccess shouldBe true
        }
        val expected = before.toSeq.sortBy(_._1).takeRight(toStore).toMap

        db.pruneSnapshots(toStore)

        db.readSnapshotsInfo.availableManifests.mapValues(bytesToId) shouldBe expected.mapValues(bytesToId)
        before.foreach { case (height, id) =>
          db.readManifestBytes(id).isDefined shouldBe expected.contains(height)
        }
      } finally {
        try destination.close()
        finally source.close()
      }
    }
  }
}
