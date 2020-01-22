package org.ergoplatform.db

import com.google.common.primitives.Ints
import org.ergoplatform.Utils
import org.ergoplatform.settings.Algos
import scorex.testkit.utils.FileUtils
import scorex.util.Random
import scorex.db.LDBVersionedStore

object LDBVersionedStoreRollbackBench extends App with FileUtils {

  private val store = new LDBVersionedStore(createTempDir, keepVersions = 400)

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
    store.update(version, toRemove, toInsert)
  }

  private val version = store.rollbackVersions().head

  val et = Utils.time {
    val result = store.rollbackTo(version)
    require(result.isSuccess)
  }

  println(s"Performance of `LDBVersionedStore.rollback (max depth)` ($numEpochs epochs): $et ms")

}
