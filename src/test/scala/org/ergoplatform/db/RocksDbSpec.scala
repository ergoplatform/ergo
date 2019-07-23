package org.ergoplatform.db

import akka.util.ByteString
import org.scalatest.{Matchers, PropSpec}
import org.rocksdb.{Options, RocksDB, WriteBatch, WriteOptions}

class RocksDbSpec extends PropSpec with DBSpec with Matchers {

  private val (keyA, valA) = (byteString("A"), byteString("1"))
  private val (keyB, valB) = (byteString("B"), byteString("2"))
  private val (keyC, valC) = (byteString("C"), byteString("3"))
  private val (keyD, valD) = (byteString("D"), byteString("4"))
  private val (keyE, valE) = (byteString("E"), byteString("5"))

  property("sample") {
    RocksDB.loadLibrary()
    val options = new Options().setCreateIfMissing(true)
    val db = RocksDB.open(options, createTempDir.getAbsolutePath)

    val wo = new WriteOptions()
    val batch = new WriteBatch()

    batch.put(keyA, valA)
    batch.put(keyC, valC)
    batch.put(keyA, valB)

    db.write(wo, batch)

    ByteString(db.get(keyC)) shouldBe ByteString(valC)
    ByteString(db.get(keyA)) shouldBe ByteString(valB)
  }

}
