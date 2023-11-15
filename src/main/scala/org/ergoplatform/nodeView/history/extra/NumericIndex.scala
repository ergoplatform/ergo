package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ErgoSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}

/**
  * Numeric index pointing to a transaction id.
  * @param n - index number of a transaction
  * @param m - id of a transaction
  */
case class NumericTxIndex(n: Long, m: ModifierId) extends ExtraIndex {
  override lazy val id: ModifierId = bytesToId(serializedId)
  override def serializedId: Array[Byte] = NumericTxIndex.indexToBytes(n)
}

object NumericTxIndexSerializer extends ErgoSerializer[NumericTxIndex] {

  override def serialize(ni: NumericTxIndex, w: Writer): Unit = {
    w.putLong(ni.n)
    w.putBytes(fastIdToBytes(ni.m))
  }

  override def parse(r: Reader): NumericTxIndex = {
    val n: Long = r.getLong()
    val m: ModifierId = bytesToId(r.getBytes(32))
    NumericTxIndex(n, m)
  }
}

object NumericTxIndex {
  val extraIndexTypeId: ExtraIndexTypeId = 25.toByte

  /**
    * Convert the index number of a transaction to an id for database retreival.
    * @param n - index number to hash
    * @return id corresponding to index number
    */
  def indexToBytes(n: Long): Array[Byte] = Algos.hash("txns height " + n)

  /**
    * Get a body-less transaction from database by its index number.
    * @param history - database handle
    * @param n       - index number of a transaction
    * @return transaction with given index, if found
    */
  def getTxByNumber(history: ErgoHistoryReader, n: Long): Option[IndexedErgoTransaction] =
    history.typedExtraIndexById[IndexedErgoTransaction](history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(n))).get.m)
}

/**
  * Numeric index pointing to a box id.
  * @param n - index number of a box
  * @param m - id of a box
  */
case class NumericBoxIndex(n: Long, m: ModifierId) extends ExtraIndex {
  override lazy val id: ModifierId = bytesToId(serializedId)
  override def serializedId: Array[Byte] = NumericBoxIndex.indexToBytes(n)
}

object NumericBoxIndexSerializer extends ErgoSerializer[NumericBoxIndex] {

  override def serialize(ni: NumericBoxIndex, w: Writer): Unit = {
    w.putLong(ni.n)
    w.putBytes(fastIdToBytes(ni.m))
  }

  override def parse(r: Reader): NumericBoxIndex = {
    val n: Long = r.getLong()
    val m: ModifierId = bytesToId(r.getBytes(32))
    NumericBoxIndex(n, m)
  }
}

object NumericBoxIndex {
  val extraIndexTypeId: ExtraIndexTypeId = 30.toByte

  /**
    * Convert the index number of a box to an id for database retreival.
    * @param n - index number to hash
    * @return id corresponding to index number
    */
  def indexToBytes(n: Long): Array[Byte] = Algos.hash("boxes height " + n)

  /**
    * Get a box from database by its index number.
    * @param history - database handle
    * @param n       - index number of a box, can be negative if box is spent
    * @return box with given index, if found
    */
  def getBoxByNumber(history: ErgoHistoryReader, n: Long): Option[IndexedErgoBox] =
    history.typedExtraIndexById[IndexedErgoBox](
      history.typedExtraIndexById[NumericBoxIndex](
        bytesToId(NumericBoxIndex.indexToBytes(math.abs(n)))
      ).get.m
    )
}
