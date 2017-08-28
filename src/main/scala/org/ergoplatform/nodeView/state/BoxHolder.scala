package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendNoncedBox

import scala.collection.immutable.SortedMap


/**
  * Unauthenticated ordered in-memory box storage. Useful to store genesis state and also for tests.
  * Not suitable for big collections.
  *
  * @param boxes - immutable sorted collection of boxes (organized as box.key -> box map)
  */
class BoxHolder(private val boxes: SortedMap[ByteArrayWrapper, AnyoneCanSpendNoncedBox]) {

  def get(id: ByteArrayWrapper): Option[AnyoneCanSpendNoncedBox] = boxes.get(id)

  def removeBoxes(ids: Seq[ByteArrayWrapper]): Unit =
    new BoxHolder(boxes.filterKeys(k => !ids.contains(k)))

  def addBoxes(bs: Seq[AnyoneCanSpendNoncedBox]): BoxHolder =
    new BoxHolder(boxes ++ bs.map(b => ByteArrayWrapper(b.id) -> b))

  def take(howMany: Int): (Seq[AnyoneCanSpendNoncedBox], BoxHolder) =
    (boxes.take(howMany).values.toSeq, new BoxHolder(boxes.drop(howMany)))

  def sortedBoxes: Set[AnyoneCanSpendNoncedBox] = boxes.keySet.map(k => boxes(k))

  override def toString = s"BoxHolder(${boxes.size} boxes inside)"
}

object BoxHolder {
  def apply(initialBoxes: Seq[AnyoneCanSpendNoncedBox]): BoxHolder =
    new BoxHolder(SortedMap(initialBoxes.map(b => ByteArrayWrapper(b.id) -> b): _*))
}