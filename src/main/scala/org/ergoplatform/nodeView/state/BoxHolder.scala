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
class BoxHolder(val boxes: SortedMap[ByteArrayWrapper, AnyoneCanSpendNoncedBox]) {

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

/**
  * For tests, box holder with in-memory diffs
  */
class VersionedInMemoryBoxHolder( override val boxes: SortedMap[ByteArrayWrapper, AnyoneCanSpendNoncedBox],
                                  val versions: IndexedSeq[ByteArrayWrapper],
                                  val diffs: Map[ByteArrayWrapper, (Seq[AnyoneCanSpendNoncedBox], Seq[AnyoneCanSpendNoncedBox])]
                                ) extends BoxHolder(boxes) {

  //todo: this implementation assumes that all the boxes in "toAdd" are not referenced by "toRemove" elements
  //todo: (so we can not handle situation when some transaction in a block spends a box within the same block)
  def applyChanges(version: ByteArrayWrapper,
                   toRemove: Seq[ByteArrayWrapper],
                   toAdd: Seq[AnyoneCanSpendNoncedBox]): VersionedInMemoryBoxHolder = {
    val newVersions = versions :+ version
    val newDiffs = diffs.updated(version, toRemove.map(k => boxes(k)) -> toAdd)
    val newBoxes = boxes -- toRemove ++ toAdd.map(box => ByteArrayWrapper(box.id) -> box)
    new VersionedInMemoryBoxHolder(newBoxes, newVersions, newDiffs)
  }

  //todo: the same issue as in applyChanges()
  def rollback(version: ByteArrayWrapper): VersionedInMemoryBoxHolder = {
    val idx = versions.indexOf(version)
    val (remainingVersions, versionsToRollback) =  versions.splitAt(idx + 1)
    val (newBoxes, newDiffs) = versionsToRollback.reverse.foldLeft(boxes -> diffs){case ((bs, ds), ver) =>
      val diff = diffs(ver)
        val removedBoxes = diff._1
        val addedBoxes = diff._2
      (bs ++ removedBoxes.map(box => ByteArrayWrapper(box.id) -> box)
            -- addedBoxes.map(_.id).map(ByteArrayWrapper.apply)) -> (ds - ver)
    }
    new VersionedInMemoryBoxHolder(newBoxes, remainingVersions, newDiffs)
  }
}

object BoxHolder {
  def apply(initialBoxes: Seq[AnyoneCanSpendNoncedBox]): BoxHolder =
    new BoxHolder(SortedMap(initialBoxes.map(b => ByteArrayWrapper(b.id) -> b): _*))
}