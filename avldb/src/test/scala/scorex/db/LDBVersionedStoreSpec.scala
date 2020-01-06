package scorex.db

import com.google.common.primitives.Longs
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.authds.avltree.batch.benchmark.LDBVersionedStoreBenchmark.getRandomTempDir

import scala.collection.mutable
import scala.util.Random

//todo: rollbacks and pruning are checked in VersionedStoreSpec, merge both tests?
class LDBVersionedStoreSpec extends PropSpec with Matchers {

  private val dir = getRandomTempDir
  private val store = new LDBVersionedStore(dir, 100)

  property("last version correct && versionIdExists && rollbackVersions") {
    val versionNum = Random.nextInt().toLong
    val versionId = Longs.toByteArray(versionNum)

    store.update(versionId, Seq.empty, Seq(versionId -> versionId))

    store.lastVersionID.get.sameElements(versionId) shouldBe true

    val versionId2 = Longs.toByteArray(versionNum + 1)

    store.update(versionId2, Seq(versionId), Seq.empty)

    store.lastVersionID.get.sameElements(versionId2) shouldBe true

    val versionId3 = Longs.toByteArray(versionNum + 3)

    store.update(versionId3, Seq.empty, Seq(versionId -> versionId))

    store.lastVersionID.get.sameElements(versionId3) shouldBe true

    store.versionIdExists(versionId) shouldBe true

    store.versionIdExists(versionId2) shouldBe true

    store.versionIdExists(versionId3) shouldBe true

    //versions are coming in recent-first order
    store.rollbackVersions().toSeq.map(_.toSeq) shouldBe Seq(versionId3.toSeq, versionId2.toSeq, versionId.toSeq)
  }

  property("processAll && getWithFilter") {
    val version = Longs.toByteArray(Long.MaxValue)
    val k1 = Longs.toByteArray(Int.MaxValue + 1)
    val k2 = Longs.toByteArray(Int.MaxValue + 2)
    val v1 = Longs.toByteArray(Int.MaxValue + 100)
    val v2 = Longs.toByteArray(Int.MaxValue + 200)
    store.update(version, Seq.empty, Seq(k1 -> v1, k2 -> v2))

    //read all keys
    val keys = store.getWithFilter((_, _) => true).toSeq.map(_._1)

    val ks = keys.map(_.toSeq)
    ks.contains(k1.toSeq) shouldBe true
    ks.contains(k2.toSeq) shouldBe true

    val buffer = mutable.Buffer[Seq[Byte]]()

    //extract certain values via processAll, also count all the items in the versioned collection
    var cnt0 = 0
    store.processAll({ case (k, v) =>
      cnt0 = cnt0 + 1
      if(k.toSeq == k1.toSeq || k.toSeq == k2.toSeq) buffer += v.toSeq
    })
    cnt0 >= 2 shouldBe true
    buffer.length shouldBe 2
    buffer.contains(v1.toSeq) shouldBe true
    buffer.contains(v2.toSeq) shouldBe true

    store.update(Longs.toByteArray(Long.MinValue), keys , Seq.empty)

    store.getWithFilter((_, _) => true).toSeq.length shouldBe 0

    var cnt = 0
    store.processAll({ case (k, v) =>
      cnt = cnt + 1
    })
    cnt shouldBe 0
  }

}
