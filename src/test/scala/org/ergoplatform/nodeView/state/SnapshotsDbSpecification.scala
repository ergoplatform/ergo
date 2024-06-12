package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.FileUtils
import org.scalacheck.Gen
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Random

class SnapshotsDbSpecification extends ErgoCorePropertyTest with FileUtils {
    import org.ergoplatform.utils.generators.CoreObjectGenerators._

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
}
