package org.ergoplatform.nodeView


import java.io.File

import akka.actor.ActorRef
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.ErgoBox
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state._
import scorex.core.{TransactionsCarryingPersistentNodeViewModifier, VersionTag}

import scala.util.{Failure, Success, Try}


class WrappedUtxoState(override val version: VersionTag,
                       store: Store,
                       val versionedBoxHolder: VersionedInMemoryBoxHolder,
                       constants: StateConstants)
  extends UtxoState(version, store, constants) {

  private type TCPMOD =
    TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction]

  def size: Int = versionedBoxHolder.size

  def takeBoxes(count: Int): Seq[ErgoBox] = versionedBoxHolder.take(count)._1

  override def rollbackTo(version: VersionTag): Try[WrappedUtxoState] = super.rollbackTo(version) match {
    case Success(us) =>
      val updHolder = versionedBoxHolder.rollback(ByteArrayWrapper(us.version))
      Success(new WrappedUtxoState(version, us.store, updHolder, constants))
    case Failure(e) => Failure(e)
  }

  override def applyModifier(mod: ErgoPersistentModifier): Try[WrappedUtxoState] = super.applyModifier(mod) match {
    case Success(us) =>
      mod match {
        case ct: TCPMOD =>
          val changes = ErgoState.stateChanges(ct.transactions)
          val updHolder = versionedBoxHolder.applyChanges(
            ByteArrayWrapper(us.version),
            changes.toRemove.map(_.boxId).map(ByteArrayWrapper.apply),
            changes.toAppend.map(_.box))
          Success(new WrappedUtxoState(VersionTag @@ mod.id, us.store, updHolder, constants))
        case _ =>
          val updHolder = versionedBoxHolder.applyChanges(ByteArrayWrapper(us.version), Seq(), Seq())
          Success(new WrappedUtxoState(VersionTag @@ mod.id, us.store, updHolder, constants))
      }
    case Failure(e) => Failure(e)
  }
}

object WrappedUtxoState {
  def apply(boxHolder: BoxHolder,
            dir: File,
            emission: CoinsEmission,
            nodeViewHolderRef: Option[ActorRef]): WrappedUtxoState = {
    val us = UtxoState.fromBoxHolder(boxHolder, dir, emission, nodeViewHolderRef)
    val constants: StateConstants = StateConstants(nodeViewHolderRef, emission)
    WrappedUtxoState(us, boxHolder, constants)
  }

  def apply(us: UtxoState, boxHolder: BoxHolder, constants: StateConstants): WrappedUtxoState = {
    val boxes = boxHolder.boxes

    val version = ByteArrayWrapper(us.version)
    val vbh = new VersionedInMemoryBoxHolder(
      boxes,
      IndexedSeq(version),
      Map(version -> (Seq() -> boxHolder.sortedBoxes.toSeq)))

    new WrappedUtxoState(ErgoState.genesisStateVersion, us.store, vbh, constants)
  }
}