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
case class BoxHolder(private val boxes: SortedMap[ByteArrayWrapper, AnyoneCanSpendNoncedBox]) {

  def removeBoxes(ids: Seq[ByteArrayWrapper]): Unit =
    BoxHolder(boxes.filterKeys(k => !ids.contains(k)))

  def addBoxes(bs: Seq[AnyoneCanSpendNoncedBox]): BoxHolder =
    BoxHolder(boxes ++ bs.map(b => ByteArrayWrapper(b.id) -> b))

  def take(howMany: Int): (Seq[AnyoneCanSpendNoncedBox], BoxHolder) =
    (boxes.take(howMany).values.toSeq, BoxHolder(boxes.drop(howMany)))

  def sortedBoxes: Set[AnyoneCanSpendNoncedBox] = boxes.keySet.map(k => boxes(k))
}

object BoxHolder {
  def apply(initialBoxes: Seq[AnyoneCanSpendNoncedBox]): BoxHolder =
    BoxHolder(SortedMap(initialBoxes.map(b => ByteArrayWrapper(b.id) -> b): _*))
}