package org.ergoplatform.db


import org.iq80.leveldb.DB

class HybridLDBKVStore(protected override val db: DB, keepVersions: Int)
  extends VersionedLDBKVStore(db, keepVersions) {

  private val cacheDb = new LDBKVStore(db)

  def cachePut(values: Seq[(K, V)]) = cacheDb.insert(values)

  def cacheRemove(keys: Seq[K]) = cacheDb.remove(keys)

}
