package org.ergoplatform.nodeView.state.wrapped

import java.io.File

import akka.actor.ActorRef
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.settings.Algos.HF
import scorex.core.{TransactionsCarryingPersistentNodeViewModifier, VersionTag, idToVersion}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.Digest32
import scorex.db.{ByteArrayWrapper, LDBVersionedStore}

import scala.util.{Failure, Success, Try}

class WrappedUtxoState(prover: PersistentBatchAVLProver[Digest32, HF],
                       override val version: VersionTag,
                       store: LDBVersionedStore,
                       val versionedBoxHolder: VersionedInMemoryBoxHolder,
                       constants: StateConstants)
  extends UtxoState(prover, version, store, constants) {

  def size: Int = versionedBoxHolder.size

  def takeBoxes(count: Int): Seq[ErgoBox] = versionedBoxHolder.take(count)._1

  override def rollbackTo(version: VersionTag): Try[WrappedUtxoState] = super.rollbackTo(version) match {
    case Success(us) =>
      val updHolder = versionedBoxHolder.rollback(us.version)
      Success(new WrappedUtxoState(us.persistentProver, version, us.store, updHolder, constants))
    case Failure(e) => Failure(e)
  }

  override def applyModifier(mod: ErgoPersistentModifier): Try[WrappedUtxoState] = super.applyModifier(mod) match {
    case Success(us) =>
      mod match {
        case ct: TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction@unchecked] =>
          // You can not get block with transactions not being of ErgoTransaction type so no type checks here.

          val changes = ErgoState.stateChanges(ct.transactions)
          val updHolder = versionedBoxHolder.applyChanges(
            us.version,
            changes.toRemove.map(_.boxId).map(ByteArrayWrapper.apply),
            changes.toAppend.map(_.box))
          Success(new WrappedUtxoState(us.persistentProver, idToVersion(mod.id), us.store, updHolder, constants))
        case _ =>
          val updHolder = versionedBoxHolder.applyChanges(us.version, Seq(), Seq())
          Success(new WrappedUtxoState(us.persistentProver, idToVersion(mod.id), us.store, updHolder, constants))
      }
    case Failure(e) => Failure(e)
  }
}

object WrappedUtxoState {

  def apply(boxHolder: BoxHolder,
            dir: File,
            nodeViewHolderRef: Option[ActorRef],
            settings: ErgoSettings): WrappedUtxoState = {
    val constants = StateConstants(nodeViewHolderRef, settings)
    val emissionBox = ErgoState.genesisBoxes(constants.settings.chainSettings).headOption
    val us = UtxoState.fromBoxHolder(boxHolder, emissionBox, dir, constants)
    WrappedUtxoState(us, boxHolder, constants)
  }

  def apply(us: UtxoState, boxHolder: BoxHolder, constants: StateConstants): WrappedUtxoState = {
    val boxes = boxHolder.boxes

    val version = us.version
    val vbh = new VersionedInMemoryBoxHolder(
      boxes,
      IndexedSeq(version),
      Map(version -> (Seq() -> boxHolder.sortedBoxes.toSeq))
    )

    new WrappedUtxoState(us.persistentProver, ErgoState.genesisStateVersion, us.store, vbh, constants)
  }
}