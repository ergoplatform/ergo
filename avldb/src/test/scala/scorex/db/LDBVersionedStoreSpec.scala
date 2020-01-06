package scorex.db

import com.google.common.primitives.Longs
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.authds.avltree.batch.benchmark.LDBVersionedStoreBenchmark.getRandomTempDir

import scala.util.Random

//todo: rollbacks and pruning are checked in VersionedStoreSpec, merge both tests?
class LDBVersionedStoreSpec extends PropSpec with Matchers {

  private val dir = getRandomTempDir
  private val store = new LDBVersionedStore(dir, 100)

  property("last version correct") {
    val versionId = Longs.toByteArray(Random.nextLong())

    store.update(versionId, Seq.empty, Seq(versionId -> versionId))

    store.lastVersionID.get.sameElements(versionId) shouldBe true

    val versionId2 = Longs.toByteArray(Random.nextLong())

    store.update(versionId2, Seq(versionId), Seq.empty)

    store.lastVersionID.get.sameElements(versionId2) shouldBe true
  }

}
