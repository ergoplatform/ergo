package org.ergoplatform.db


import java.io.File

import scorex.db.{LDBKVStore, LDBVersionedStore}

class HybridLDBKVStore(protected override val dir: File, keepVersions: Int)
  extends LDBVersionedStore(dir, keepVersions) {

  private val cacheDb = new LDBKVStore(db)

  def cachePut(values: Seq[(K, V)]): Unit = cacheDb.insert(values)

  def cacheRemove(keys: Seq[K]): Unit = cacheDb.remove(keys)

}
