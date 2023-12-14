package org.ergoplatform.nodeView.state.wrapped

import java.io.File
import akka.actor.ActorRef
import org.ergoplatform.{ErgoBox, TransactionsCarryingPersistentNodeViewModifier}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.core.{VersionTag, idToVersion}
import org.ergoplatform.nodeView.LocallyGeneratedModifier
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.Digest32
import scorex.db.{ByteArrayWrapper, LDBVersionedStore}

import scala.util.{Failure, Success, Try}

class WrappedUtxoState(prover: PersistentBatchAVLProver[Digest32, HF],
                       override val version: VersionTag,
                       store: LDBVersionedStore,
                       val versionedBoxHolder: VersionedInMemoryBoxHolder,
                       settings: ErgoSettings)
  extends UtxoState(prover, version, store, settings) {

  def size: Int = versionedBoxHolder.size

  def takeBoxes(count: Int): Seq[ErgoBox] = versionedBoxHolder.take(count)._1

  override def rollbackTo(version: VersionTag): Try[WrappedUtxoState] = super.rollbackTo(version) match {
    case Success(us) =>
      val updHolder = versionedBoxHolder.rollback(us.version)
      Success(new WrappedUtxoState(us.persistentProver, version, us.store, updHolder, settings))
    case Failure(e) => Failure(e)
  }

  override def applyModifier(mod: BlockSection, estimatedTip: Option[Height] = None)
                            (generate: LocallyGeneratedModifier => Unit): Try[WrappedUtxoState] =
    super.applyModifier(mod, estimatedTip)(generate) match {
      case Success(us) =>
        mod match {
          case ct: TransactionsCarryingPersistentNodeViewModifier =>
            // You can not get block with transactions not being of ErgoTransaction type so no type checks here.

            val changes = ErgoState.stateChanges(ct.transactions).get
            val updHolder = versionedBoxHolder.applyChanges(
              us.version,
              changes.toRemove.map(_.key).map(ByteArrayWrapper.apply),
              changes.toAppend.map(_.value).map(ErgoBoxSerializer.parseBytes))
            Success(new WrappedUtxoState(us.persistentProver, idToVersion(mod.id), us.store, updHolder, settings))
          case _ =>
            val updHolder = versionedBoxHolder.applyChanges(us.version, Seq(), Seq())
            Success(new WrappedUtxoState(us.persistentProver, idToVersion(mod.id), us.store, updHolder, settings))
        }
      case Failure(e) => Failure(e)
    }
}

object WrappedUtxoState {

  def apply(boxHolder: BoxHolder,
            dir: File,
            nodeViewHolderRef: Option[ActorRef],
            parameters: Parameters,
            settings: ErgoSettings): WrappedUtxoState = {
    val emissionBox = ErgoState.genesisBoxes(settings.chainSettings).headOption
    val us = UtxoState.fromBoxHolder(boxHolder, emissionBox, dir, settings, parameters)
    WrappedUtxoState(us, boxHolder, settings, parameters)
  }

  def apply(us: UtxoState, boxHolder: BoxHolder, settings: ErgoSettings, parameters: Parameters): WrappedUtxoState = {
    val boxes = boxHolder.boxes

    val version = us.version
    val vbh = new VersionedInMemoryBoxHolder(
      boxes,
      IndexedSeq(version),
      Map(version -> (Seq() -> boxHolder.sortedBoxes.toSeq))
    )

    new WrappedUtxoState(us.persistentProver, ErgoState.genesisStateVersion, us.store, vbh, settings)
  }
}
