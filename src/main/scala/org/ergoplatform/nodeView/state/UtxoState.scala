package org.ergoplatform.nodeView.state

import java.io.File

import akka.actor.ActorRef
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.VersionTag
import scorex.core.transaction.state.TransactionValidation
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADValue, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation.
  *
  * @param store - database where persistent UTXO set authenticated with the help of an AVL+ tree is residing
  */
class UtxoState(override val version: VersionTag,
                override val store: Store,
                nodeViewHolderRef: Option[ActorRef])
  extends ErgoState[UtxoState]
    with TransactionValidation[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction]
    with UtxoStateReader {

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if (nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    nodeViewHolderRef.foreach(h => h ! LocallyGeneratedModifier(proof))
  }

  import UtxoState.metadata

  override val maxRollbackDepth = 10

  override lazy val rootHash: ADDigest = persistentProver.digest

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    val p = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    store.get(ByteArrayWrapper(version)) match {
      case Some(hash) =>
        val rollbackResult = p.rollback(ADDigest @@ hash.data).map { _ =>
          new UtxoState(version, store, nodeViewHolderRef) {
            override protected lazy val persistentProver = p
          }
        }
        store.clean(ErgoState.KeepVersions)
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  @SuppressWarnings(Array("TryGet"))
  private[state] def applyTransactions(transactions: Seq[AnyoneCanSpendTransaction], expectedDigest: ADDigest) = Try {

    transactions.foreach(tx => tx.semanticValidity.get)

    val mods = boxChanges(transactions).operations.map(ADProofs.changeToMod)
    mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
      t.flatMap(_ => {
        persistentProver.performOneOperation(m)
      })
    }.get

    if (!expectedDigest.sameElements(persistentProver.digest)) {
      throw new Error(s"Digest after txs application is wrong. ${Algos.encode(expectedDigest)} expected, " +
        s"${Algos.encode(persistentProver.digest)} given")
    }
  }

  //todo: utxo snapshot could go here
  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] = mod match {
    case fb: ErgoFullBlock =>
      log.debug(s"Trying to apply full block with header ${fb.header.encodedId} at height ${fb.header.height} " +
        s"to UtxoState with root hash ${Algos.encode(rootHash)}")

      val stateTry: Try[UtxoState] = applyTransactions(fb.blockTransactions.txs, fb.header.stateRoot) map { _: Unit =>
        val md = metadata(VersionTag @@ fb.id, fb.header.stateRoot)
        val proofBytes = persistentProver.generateProofAndUpdateStorage(md)
        val proofHash = ADProofs.proofDigest(proofBytes)
        if (fb.aDProofs.isEmpty) onAdProofGenerated(ADProofs(fb.header.id, proofBytes))
        log.info(s"Valid modifier ${fb.encodedId} with header ${fb.header.encodedId} applied to UtxoState with " +
          s"root hash ${Algos.encode(rootHash)}")
        if (!store.get(ByteArrayWrapper(fb.id)).exists(_.data sameElements fb.header.stateRoot)) {
          throw new Error("Storage kept roothash is not equal to the declared one")
        } else if (!(fb.header.ADProofsRoot sameElements proofHash)) {
          throw new Error("Calculated proofHash is not equal to the declared one")
        } else if (!(fb.header.stateRoot sameElements persistentProver.digest)) {
          throw new Error("Calculated stateRoot is not equal to the declared one")
        }
        new UtxoState(VersionTag @@ fb.id, store, nodeViewHolderRef)
      }
      stateTry.recoverWith[UtxoState] { case e =>
        log.warn(s"Error while applying full block with header ${fb.header.encodedId} to UTXOState with root" +
          s" ${Algos.encode(rootHash)}: ", e)
        persistentProver.rollback(rootHash).ensuring(persistentProver.digest.sameElements(rootHash))
        Failure(e)
      }

    case h: Header =>
      Success(new UtxoState(VersionTag @@ h.id, this.store, nodeViewHolderRef))

    case a: Any =>
      log.info(s"Unhandled modifier: $a")
      Failure(new Exception("unknown modifier"))
  }

  @SuppressWarnings(Array("OptionGet"))
  override def rollbackVersions: Iterable[VersionTag] = persistentProver.storage.rollbackVersions.map { v =>
    VersionTag @@ store.get(ByteArrayWrapper(Algos.hash(v))).get.data
  }

}

object UtxoState {
  private lazy val bestVersionKey = Algos.hash("best state version")

  private def metadata(modId: VersionTag, stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
  }

  def create(dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val store = new LSMStore(dir, keepVersions = ErgoState.KeepVersions) // todo: magic number, move to settings
    val dbVersion = store.get(ByteArrayWrapper(bestVersionKey)).map(VersionTag @@ _.data)
    new UtxoState(dbVersion.getOrElse(ErgoState.genesisStateVersion), store, nodeViewHolderRef)
  }

  @SuppressWarnings(Array("OptionGet", "TryGet"))
  def fromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize))
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val store = new LSMStore(dir, keepVersions = ErgoState.KeepVersions) // todo: magic number, move to settings

    new UtxoState(ErgoState.genesisStateVersion, store, nodeViewHolderRef) {
      override protected lazy val persistentProver =
        PersistentBatchAVLProver.create(
          p,
          storage,
          metadata(ErgoState.genesisStateVersion, p.digest),
          paranoidChecks = true
        ).get

      assert(persistentProver.digest.sameElements(storage.version.get))
    }
  }
}
