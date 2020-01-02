package org.ergoplatform.db

import com.google.common.primitives.Ints
import org.ergoplatform.Utils
import org.ergoplatform.settings.Algos
import org.iq80.leveldb.Options
import scorex.testkit.utils.FileUtils
import scorex.util.Random
import scorex.db.LDBFactory.factory

object VLDBStoreRollbackBench
  extends App
    with FileUtils {

  private val options = new Options()
  options.createIfMissing(true)
  private val db0 = factory.open(createTempDir, options)

  private val store = new VersionedLDBKVStore(db0, keepVersions = 400)

  private val numEpochs = 10000
  private val elemsAtStep = 500

  private def key(i: Int): Array[Byte] = Algos.hash(Ints.toByteArray(i))

  (0 to numEpochs).foreach { i =>
    val toInsert = (0 to elemsAtStep).map { i0 =>
      key(i0 + i * elemsAtStep) -> Random.randomBytes(1000)
    }
    val toRemove =
      if (i > 1) (0 to (elemsAtStep / 2)).map(_ => key(scala.util.Random.nextInt((i - 1) * elemsAtStep))) else Seq.empty
    val version = Algos.hash(key(i))
    store.update(toInsert, toRemove)(version)
  }

  private val version = store.versions.last

  val et = Utils.time {
    val result = store.rollbackTo(version)
    require(result.isSuccess)
  }

  println(s"Performance of `VersionedLDB.rollback (max depth)` ($numEpochs epochs): $et ms")

}
