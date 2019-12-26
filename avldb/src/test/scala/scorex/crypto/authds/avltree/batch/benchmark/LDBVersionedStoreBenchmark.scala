package scorex.crypto.authds.avltree.batch.benchmark

import com.google.common.primitives.Longs
import scorex.crypto.authds.avltree.batch.helpers.FileHelper
import scorex.utils.Random
import scorex.db.LDBVersionedStore

object LDBVersionedStoreBenchmark extends App with FileHelper {
  val KL = 32
  val VL = 8
  val LL = 32
  val NumMods = 2000000
  val Step = 1000

  val store = new LDBVersionedStore(getRandomTempDir, 10)
  val mods = generateModifications()
  var currentVersion: Option[Long] = None

  (0 until(NumMods, Step)) foreach { i =>
    println(i)
    val mod: Seq[(Array[Byte], Array[Byte])] = mods.slice(i, i + Step)
    val nextVersion = Longs.toByteArray(i)
    store.update(nextVersion, Seq(), mod)
    currentVersion.foreach(v => {
      store.rollback(Longs.toByteArray(v))
      store.update(nextVersion, Seq(), mod)
    })
    currentVersion = Some(i)

    mods.slice(0, i + Step).foreach { m =>
      store(m._1)
    }
  }

  def generateModifications(): Array[(Array[Byte], Array[Byte])] = {
    val mods = new Array[(Array[Byte], Array[Byte])](NumMods)

    for (i <- 0 until NumMods) {
      mods(i) = (Random.randomBytes(KL), Random.randomBytes(VL))
    }
    mods
  }
}
