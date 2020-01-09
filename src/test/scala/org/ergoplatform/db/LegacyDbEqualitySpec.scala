package org.ergoplatform.db

import akka.util.ByteString
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalatest.{Matchers, PropSpec}
import scorex.db.LDBVersionedStore

/**
  * LSMStore and VersionedLDBKVStore ops equality checks
  */
class LegacyDbEqualitySpec extends PropSpec with DBSpec with Matchers {

  private val (keyA, valA) = (byteString32("A"), byteString("1"))
  private val (keyB, valB) = (byteString32("B"), byteString("2"))
  private val (keyC, valC) = (byteString32("C"), byteString("3"))
  private val (keyD, valD) = (byteString32("D"), byteString("4"))
  private val (keyE, valE) = (byteString32("E"), byteString("5"))

  private val v1 = versionId("v1")
  private val v2 = versionId("v2")
  private val v3 = versionId("v3")

  implicit class OptValueOpsBaw(x: Option[Array[Byte]]) {
    def toBaw: Option[ByteArrayWrapper] = x.map(ByteArrayWrapper.apply)
  }

  implicit class ValueOpsBaw(x: Array[Byte]) {
    def toBaw: ByteArrayWrapper = ByteArrayWrapper(x)
  }

  implicit class KeyValueOpsBaw(xs: Seq[(Array[Byte], Array[Byte])]) {
    def toBaw: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = xs.map(x => ByteArrayWrapper(x._1) -> ByteArrayWrapper(x._2))
  }

  private def withDbs(body: (LDBVersionedStore, LSMStore) => Unit): Unit =
    withVersionedStore(100) {
      body(_, new LSMStore(createTempDir))
    }

  property("update/read roundtrip equality") {
    withDbs { (ldb, iodb) =>
      val toInsert0 = Seq(keyA -> valA, keyB -> valB)
      val toInsert1 = Seq(keyA -> valC)

      ldb.insert(v1, toInsert0)
      iodb.update(v1.toBaw, Seq.empty, toInsert0.toBaw)

      val ldbContents = ldb.get(toInsert0.map(_._1))
        .collect { case (k, Some(v)) => k -> v }
        .toSeq
        .toBs
      val iodbContents = iodb.get(toInsert0.toBaw.map(_._1))
        .collect { case (k, Some(v)) => k -> v }
        .map { case (k, v) => ByteString(k.data) -> ByteString(v.data) }

      iodbContents should contain theSameElementsAs ldbContents

      ldb.insert(v2, toInsert1)
      iodb.update(v2.toBaw, Seq.empty, toInsert1.toBaw)

      val ldbUpdatedValue = ldb.get(keyA)
      val iodbUpdatedValue = iodb.get(ByteArrayWrapper(keyA))

      ldbUpdatedValue.toBs shouldEqual iodbUpdatedValue.map(x => ByteString(x.data))
    }
  }

  property("rollback equality") {
    withDbs { (ldb, iodb) =>
      val toInsert0 = Seq(keyA -> valA, keyB -> valB)
      val toInsert1 = Seq(keyA -> valC, keyD -> valD)
      val toInsert2 = Seq(keyC -> valC, keyE -> valE)

      Seq(v1 -> toInsert0, v2 -> toInsert1, v3 -> toInsert2).foreach { case (v, toInsert) =>
        ldb.insert(v, toInsert)
        iodb.update(v.toBaw, Seq.empty, toInsert.toBaw)
      }

      val finalState = toInsert0.tail ++ toInsert1 ++ toInsert2
      val ldbContents = ldb.get(finalState.map(_._1))
        .collect { case (k, Some(v)) => k -> v }
        .toSeq
        .toBs
      val iodbContents = iodb.get(finalState.toBaw.map(_._1))
        .collect { case (k, Some(v)) => k -> v }
        .map { case (k, v) => ByteString(k.data) -> ByteString(v.data) }

      iodbContents should contain theSameElementsAs ldbContents

      ldb.rollback(v1)
      iodb.rollback(v1.toBaw)

      val ldbContentsRolledBack = ldb.get(toInsert0.map(_._1))
        .collect { case (k, Some(v)) => k -> v }
        .toSeq
        .toBs
      val iodbContentsRolledBack = iodb.get(toInsert0.toBaw.map(_._1))
        .collect { case (k, Some(v)) => k -> v }
        .map { case (k, v) => ByteString(k.data) -> ByteString(v.data) }

      iodbContentsRolledBack should contain theSameElementsAs ldbContentsRolledBack
    }
  }

}
