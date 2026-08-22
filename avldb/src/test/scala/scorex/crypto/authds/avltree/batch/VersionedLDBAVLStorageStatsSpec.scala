package scorex.crypto.authds.avltree.batch

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.authds.avltree.batch.benchmark.LDBVersionedStoreBenchmark.getRandomTempDir
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.LDBVersionedStore
import scorex.utils.Random

class VersionedLDBAVLStorageStatsSpec extends AnyPropSpec with Matchers {

  type HF = Blake2b256.type
  implicit val hf: HF = Blake2b256

  private val ValueSize = 100
  private val InsertCount = 200

  property("collectStats counts boxes and store records correctly") {
    val stateStore = new LDBVersionedStore(getRandomTempDir, initialKeepVersions = 10)
    val storage = new VersionedLDBAVLStorage(stateStore)
    val prover = PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None), storage).get

    (0 until InsertCount).foreach { i =>
      prover.performOneOperation(
        Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(ValueSize)(i.toByte))
      ).get
    }
    prover.generateProofAndUpdateStorage()

    val stats = storage.collectStats.get

    // every record is classified into exactly one bucket (nothing lost or double-counted)
    stats.totalRecords shouldBe (stats.leafRecords + stats.internalRecords + stats.otherRecords)
    stats.totalValueBytes shouldBe
      (stats.leafRecordBytes + stats.internalRecordBytes + stats.otherRecordBytes)

    // the live tree holds the inserted boxes plus the special (infinity) sentinel leaf
    stats.liveBoxes shouldBe (InsertCount + 1)

    // physical (single committed version) and live views agree on node counts and box bytes
    stats.leafRecords shouldBe stats.liveBoxes
    stats.internalRecords shouldBe stats.liveInternalNodes
    stats.leafValueBytes shouldBe stats.liveBoxValueBytes

    // box payload bytes account for all inserted values (the sentinel leaf may add a little)
    stats.liveBoxValueBytes should be >= (InsertCount.toLong * ValueSize)

    // metadata records (at least the two top-node index keys) are present and not miscounted as nodes
    stats.otherRecords should be >= 2L

    stats.treeHeight should be > 0

    stateStore.close()
  }
}
