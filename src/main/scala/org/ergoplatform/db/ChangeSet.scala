package org.ergoplatform.db

import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

/**
  * Describes a set of changes which could be reverted later.
  * @param insertedKeys - inserted keys
  * @param removed - removed entries (key, value)
  * @param altered - altered keys (key, oldValue)
  */
final case class ChangeSet(insertedKeys: Seq[Array[Byte]],
                           removed: Seq[(Array[Byte], Array[Byte])],
                           altered: Seq[(Array[Byte], Array[Byte])])

object ChangeSetSerializer extends ScorexSerializer[ChangeSet] {

  override def serialize(obj: ChangeSet, w: Writer): Unit = {
    w.putInt(obj.insertedKeys.size)
    obj.insertedKeys.foreach { k =>
      require(k.length <= 255, "Illegal key size")
      w.putUByte(k.length)
      w.putBytes(k)
    }
    w.putInt(x = obj.removed.size)
    obj.removed.foreach { case (k, v) =>
      require(k.length <= 255, "Illegal key size")
      w.putUByte(k.length)
      w.putBytes(k)
      w.putInt(v.length)
      w.putBytes(v)
    }
    w.putInt(obj.altered.size)
    obj.altered.foreach { case (k, oldV) =>
      require(k.length <= 255, "Illegal key size")
      w.putUByte(k.length)
      w.putBytes(k)
      w.putInt(oldV.length)
      w.putBytes(oldV)
    }
  }

  override def parse(r: Reader): ChangeSet = {
    val insertedQty = r.getInt()
    val insertedKeys = (0 until insertedQty).foldLeft(Seq.empty[Array[Byte]]) { case (acc, _) =>
      val len = r.getUByte()
      acc :+ r.getBytes(len)
    }
    val removedQty = r.getInt()
    val removed = (0 until removedQty).foldLeft(Seq.empty[(Array[Byte], Array[Byte])]) { case (acc, _) =>
      val kLen = r.getUByte()
      val k = r.getBytes(kLen)
      val vLen = r.getInt()
      val v = r.getBytes(vLen)
      acc :+ (k -> v)
    }
    val alteredQty = r.getInt()
    val altered = (0 until alteredQty).foldLeft(Seq.empty[(Array[Byte], Array[Byte])]) { case (acc, _) =>
      val kLen = r.getUByte()
      val k = r.getBytes(kLen)
      val oldVLen = r.getInt()
      val oldV = r.getBytes(oldVLen)
      acc :+ (k -> oldV)
    }
    ChangeSet(insertedKeys, removed, altered)
  }

}
