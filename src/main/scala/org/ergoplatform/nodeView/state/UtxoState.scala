package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.VersionTag
import scorex.core.transaction.state.TransactionValidation
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation.
  *
  * @param store - database where persistent UTXO set authenticated with the help of an AVL+ tree is residing
  */
class UtxoState(override val version: VersionTag,
                override val store: Store,
                override val constants: StateConstants)
  extends ErgoState[UtxoState]
    with TransactionValidation[ErgoTransaction]
    with UtxoStateReader {

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if (constants.nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    constants.nodeViewHolderRef.foreach(h => h ! LocallyGeneratedModifier(proof))
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
          new UtxoState(version, store, constants) {
            override protected lazy val persistentProver = p
          }
        }
        store.clean(constants.keepVersions)
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  @SuppressWarnings(Array("TryGet"))
  private[state] def applyTransactions(transactions: Seq[ErgoTransaction],
                                       expectedDigest: ADDigest,
                                       height: Height) = Try {

    val createdOutputs = transactions.flatMap(_.outputs).map(o => (ByteArrayWrapper(o.id), o)).toMap
    val totalCost = transactions.map { tx =>
      tx.statelessValidity.get
      val boxesToSpend = tx.inputs.map(_.boxId).map { id =>
        createdOutputs.get(ByteArrayWrapper(id)).orElse(boxById(id)) match {
          case Some(box) => box
          case None => throw new Error(s"Box with id ${Algos.encode(id)} not found")
        }
      }
      tx.statefulValidity(boxesToSpend, stateContext).get
    }.sum

    if (totalCost > Constants.MaxTransactionCost) throw new Error(s"Transaction cost $totalCost exeeds limit")

    val mods = ErgoState.stateChanges(transactions).operations.map(ADProofs.changeToMod)
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
      val height = fb.header.height

      log.debug(s"Trying to apply full block with header ${fb.header.encodedId} at height $height " +
        s"to UtxoState with root hash ${Algos.encode(rootHash)}")

      val stateTry: Try[UtxoState] = applyTransactions(fb.blockTransactions.txs, fb.header.stateRoot, height).map { _: Unit =>
        val emissionBox = extractEmissionBox(fb)
        val newStateContext = stateContext.appendHeader(fb.header)
        val md = metadata(VersionTag @@ fb.id, fb.header.stateRoot, emissionBox, newStateContext)

        val proofBytes = persistentProver.generateProofAndUpdateStorage(md)
        val proofHash = ADProofs.proofDigest(proofBytes)
        if (fb.aDProofs.isEmpty) onAdProofGenerated(ADProofs(fb.header.id, proofBytes))

        log.info(s"Valid modifier with header ${fb.header.encodedId} and emission box " +
          s"${emissionBox.map(e => Algos.encode(e.id))} applied to UtxoState with root hash ${Algos.encode(rootHash)}")
        if (!store.get(ByteArrayWrapper(fb.id)).exists(_.data sameElements fb.header.stateRoot)) {
          throw new Error("Storage kept roothash is not equal to the declared one")
        } else if (!(fb.header.ADProofsRoot sameElements proofHash)) {
          throw new Error("Calculated proofHash is not equal to the declared one")
        } else if (!(fb.header.stateRoot sameElements persistentProver.digest)) {
          throw new Error("Calculated stateRoot is not equal to the declared one")
        }
        new UtxoState(VersionTag @@ fb.id, store, constants)
      }
      stateTry.recoverWith[UtxoState] { case e =>
        log.warn(s"Error while applying full block with header ${fb.header.encodedId} to UTXOState with root" +
          s" ${Algos.encode(rootHash)}: ", e)
        persistentProver.rollback(rootHash).ensuring(persistentProver.digest.sameElements(rootHash))
        Failure(e)
      }

    case h: Header =>
      Success(new UtxoState(VersionTag @@ h.id, this.store, constants))

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
  val EmissionBoxIdKey = Algos.hash("emission box id key")

  private def metadata(modId: VersionTag,
                       stateRoot: ADDigest,
                       emissionBoxOpt: Option[ErgoBox],
                       context: ErgoStateContext): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId
    val eb = EmissionBoxIdKey -> emissionBoxOpt.map(emissionBox => emissionBox.id).getOrElse(Array[Byte]())
    val cb = ErgoStateReader.ContextKey -> context.bytes

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion, eb, cb)
  }

  def create(dir: File, constants: StateConstants): UtxoState = {
    val store = new LSMStore(dir, keepVersions = constants.keepVersions)
    val dbVersion = store.get(ByteArrayWrapper(bestVersionKey)).map(VersionTag @@ _.data)
    new UtxoState(dbVersion.getOrElse(ErgoState.genesisStateVersion), store, constants)
  }

  @SuppressWarnings(Array("OptionGet", "TryGet"))
  def fromBoxHolder(bh: BoxHolder,
                    emissionBoxOpt: Option[ErgoBox],
                    dir: File,
                    constants: StateConstants): UtxoState = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val store = new LSMStore(dir, keepVersions = constants.keepVersions)
    val defaultStateContext = ErgoStateContext(Seq(), p.digest)

    new UtxoState(ErgoState.genesisStateVersion, store, constants) {
      override protected lazy val persistentProver =
        PersistentBatchAVLProver.create(
          p,
          storage,
          metadata(ErgoState.genesisStateVersion, p.digest, emissionBoxOpt, defaultStateContext),
          paranoidChecks = true
        ).get

      assert(persistentProver.digest.sameElements(storage.version.get))
    }
  }
}
