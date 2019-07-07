package org.ergoplatform.db

import akka.util.ByteString
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

/**
  * Describes a set of changes which could be reverted later.
  * @param insertedKeys - inserted keys
  * @param removed - removed entries (key, value)
  * @param altered - altered keys (key, oldValue)
  */
final case class ChangeSet(insertedKeys: Seq[ByteString],
                           removed: Seq[(ByteString, ByteString)],
                           altered: Seq[(ByteString, ByteString)])

object ChangeSetSerializer extends ScorexSerializer[ChangeSet] {

  override def serialize(obj: ChangeSet, w: Writer): Unit = {
    w.putInt(obj.insertedKeys.size)
    obj.insertedKeys.foreach { k =>
      require(k.size <= 255, "Illegal key size")
      w.putUByte(k.size)
      w.putBytes(k.toArray)
    }
    w.putInt(obj.removed.size)
    obj.removed.foreach { case (k, v) =>
      require(k.size <= 255, "Illegal key size")
      w.putUByte(k.size)
      w.putBytes(k.toArray)
      w.putInt(v.size)
      w.putBytes(v.toArray)
    }
    w.putInt(obj.altered.size)
    obj.altered.foreach { case (k, oldV) =>
      require(k.size <= 255, "Illegal key size")
      w.putUByte(k.size)
      w.putBytes(k.toArray)
      w.putInt(oldV.size)
      w.putBytes(oldV.toArray)
    }
  }

  override def parse(r: Reader): ChangeSet = {
    val insertedQty = r.getInt()
    val insertedKeys = (0 until insertedQty).foldLeft(Seq.empty[ByteString]) { case (acc, _) =>
      val len = r.getUByte()
      acc :+ ByteString(r.getBytes(len))
    }
    val removedQty = r.getInt()
    val removed = (0 until removedQty).foldLeft(Seq.empty[(ByteString, ByteString)]) { case (acc, _) =>
      val kLen = r.getUByte()
      val k = ByteString(r.getBytes(kLen))
      val vLen = r.getInt()
      val v = ByteString(r.getBytes(vLen))
      acc :+ (k -> v)
    }
    val alteredQty = r.getInt()
    val altered = (0 until alteredQty).foldLeft(Seq.empty[(ByteString, ByteString)]) { case (acc, _) =>
      val kLen = r.getUByte()
      val k = ByteString(r.getBytes(kLen))
      val oldVLen = r.getInt()
      val oldV = ByteString(r.getBytes(oldVLen))
      acc :+ (k -> oldV)
    }
    ChangeSet(insertedKeys, removed, altered)
  }

}
