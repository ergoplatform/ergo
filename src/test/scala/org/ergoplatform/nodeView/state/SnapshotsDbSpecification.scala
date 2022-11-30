package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.crypto.hash.Digest32
import scorex.util.{idToBytes, bytesToId}
import scala.util.Random

class SnapshotsDbSpecification extends ErgoPropertyTest {
  property("snapshotsInfo round-trip") {
    forAll(Gen.nonEmptyListOf(modifierIdGen)){manifestIds =>
      val m = manifestIds.map{mid =>
        Random.nextInt(1000000) -> (Digest32 @@ idToBytes(mid))
      }.toMap
      val si = SnapshotsInfo(m)
      val dir = createTempDir.getAbsolutePath
      val db = SnapshotsDb.create(dir)
      db.writeSnapshotsInfo(si)

      val read = db.readSnapshotsInfo.availableManifests.mapValues(bs => bytesToId(bs))
      val siTocompare = si.availableManifests.mapValues(bs => bytesToId(bs))

      read shouldBe siTocompare
    }
  }
}
