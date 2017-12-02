package org.ergoplatform.nodeView


import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, UtxoState, VersionedInMemoryBoxHolder}
import scorex.core.{TransactionsCarryingPersistentNodeViewModifier, VersionTag}

import scala.util.{Failure, Success, Try}


class WrappedUtxoState(override val version: VersionTag, store: Store, generatedProofs: Seq[ADProofs],
                       val versionedBoxHolder: VersionedInMemoryBoxHolder)
  extends UtxoState(version, store, Seq()) {

  private type TCPMOD =
    TransactionsCarryingPersistentNodeViewModifier[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction]

  def size: Int = versionedBoxHolder.size

  def takeBoxes(count: Int): Seq[AnyoneCanSpendNoncedBox] = versionedBoxHolder.take(count)._1

  override def rollbackTo(version: VersionTag): Try[WrappedUtxoState] = super.rollbackTo(version) match {
    case Success(us) =>
      val updHolder = versionedBoxHolder.rollback(ByteArrayWrapper(us.version))
      Success(new WrappedUtxoState(version, us.store, us.generatedProofs, updHolder))
    case Failure(e) => Failure(e)
  }

  override def applyModifier(mod: ErgoPersistentModifier): Try[WrappedUtxoState] = super.applyModifier(mod) match {
    case Success(us) =>
      mod match {
        case ct: TCPMOD =>
          val changes = boxChanges(ct.transactions)
          val updHolder = versionedBoxHolder.applyChanges(
            ByteArrayWrapper(us.version),
            changes.toRemove.map(_.boxId).map(ByteArrayWrapper.apply),
            changes.toAppend.map(_.box))
          Success(new WrappedUtxoState(VersionTag @@ mod.id, us.store, us.generatedProofs, updHolder))
        case _ =>
          val updHolder = versionedBoxHolder.applyChanges(ByteArrayWrapper(us.version), Seq(), Seq())
          Success(new WrappedUtxoState(VersionTag @@ mod.id, us.store, us.generatedProofs, updHolder))
      }
    case Failure(e) => Failure(e)
  }
}

object WrappedUtxoState {
  def apply(boxHolder: BoxHolder, dir: File): WrappedUtxoState = {
    val us = UtxoState.fromBoxHolder(boxHolder, dir)
    WrappedUtxoState(us, boxHolder)
  }

  def apply(us: UtxoState, boxHolder: BoxHolder): WrappedUtxoState = {
    val boxes = boxHolder.boxes

    val version = ByteArrayWrapper(us.version)
    val vbh = new VersionedInMemoryBoxHolder(
      boxes,
      IndexedSeq(version),
      Map(version -> (Seq() -> boxHolder.sortedBoxes.toSeq)))

    new WrappedUtxoState(ErgoState.genesisStateVersion, us.store, us.generatedProofs, vbh)
  }
}