package org.ergoplatform.nodeView.state

import java.io.File

import cats.Traverse
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.UtxoSnapshot
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.state.UtxoState.ModifierProcessing
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings, Parameters}
import org.ergoplatform.utils.LoggingUtil
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core._
import scorex.core.transaction.state.TransactionValidation
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSerializer
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation
  *
  * @param persistentProver - persistent prover that build authenticated AVL+ tree on top of utxo set
  * @param store            - storage of persistentProver that also keeps metadata
  * @param version          - current state version
  * @param constants        - constants, that do not change with state version changes
  */
class UtxoState(override val persistentProver: PersistentBatchAVLProver[Digest32, HF],
                override val version: VersionTag,
                override val store: Store,
                override val constants: StateConstants,
                settings: ErgoSettings)
  extends ErgoState[UtxoState]
    with TransactionValidation[ErgoTransaction]
    with UtxoStateReader {

  import UtxoState.metadata

  override def rootHash: ADDigest = persistentProver.synchronized {
    persistentProver.digest
  }

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if (constants.nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    constants.nodeViewHolderRef.foreach(h => h ! LocallyGeneratedModifier(proof))
  }

  override val maxRollbackDepth = 10

  override def rollbackTo(version: VersionTag): Try[UtxoState] = persistentProver.synchronized {
    val p = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    store.get(Algos.versionToBAW(version)) match {
      case Some(hash) =>
        val rootHash: ADDigest = ADDigest @@ hash.data
        val rollbackResult = p.rollback(rootHash).map { _ =>
          new UtxoState(p, version, store, constants, settings)
        }
        store.clean(constants.keepVersions)
        rollbackResult
      case None =>
        Failure(new Exception(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  private[state] def applyTransactions(transactions: Seq[ErgoTransaction],
                                       expectedDigest: ADDigest,
                                       currentStateContext: ErgoStateContext): Try[Unit] = {
    import cats.instances.list._
    import cats.instances.try_._
    val createdOutputs = transactions.flatMap(_.outputs).map(o => (ByteArrayWrapper(o.id), o)).toMap
    val executionCostTry = Traverse[List].sequence {
      transactions.map { tx =>
        tx.statelessValidity.flatMap { _ =>
          val boxesToSpendTry = Traverse[List].sequence {
            tx.inputs.map(_.boxId).map { id =>
              createdOutputs.get(ByteArrayWrapper(id)).orElse(boxById(id))
                .map(Success.apply)
                .getOrElse(Failure(new Exception(s"Box with id ${Algos.encode(id)} not found")))
            }.toList
          }
          boxesToSpendTry.flatMap { boxes =>
            tx.statefulValidity(boxes.toIndexedSeq, currentStateContext, constants.settings.metadata)
          }
        }
      }.toList
    }.map(_.sum)
    executionCostTry match {
      case Success(executionCost) if executionCost <= Parameters.MaxBlockCost =>
        persistentProver.synchronized {
          val mods = ErgoState.stateChanges(transactions).operations.map(ADProofs.changeToMod)
          val resultTry = Traverse[List].sequence(mods.map(persistentProver.performOneOperation).toList).map(_ => ())
          if (java.util.Arrays.equals(expectedDigest, persistentProver.digest) || resultTry.isFailure) {
            resultTry
          } else {
            Failure(new Exception(s"Digest after txs application is wrong. ${Algos.encode(expectedDigest)} expected, " +
              s"${Algos.encode(persistentProver.digest)} given"))
          }
        }
      case Success(executionCost) => Failure(new Exception(s"Transaction cost $executionCost exceeds limit"))
      case failure => failure.map(_ => ())
    }
  }

  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] = processing(mod)

  private def processing: ModifierProcessing = {
    applyFullBlock orElse
      applyHeader orElse
      applySnapshot orElse
      other
  }

  private def applyFullBlock: ModifierProcessing = {
    case fb: ErgoFullBlock =>
      val height = fb.header.height

      log.debug(s"Trying to apply full block with header ${fb.header.encodedId} at height $height")
      persistentProver.synchronized {
        val inRoot = rootHash
        val newStateContext = stateContext.appendHeader(fb.header)

        val stateTry: Try[UtxoState] = applyTransactions(fb.blockTransactions.txs, fb.header.stateRoot, newStateContext).map { _: Unit =>
          val emissionBox = extractEmissionBox(fb)
          val md = metadata(idToVersion(fb.id), fb.header.stateRoot, emissionBox, newStateContext)
          val proofBytes = persistentProver.generateProofAndUpdateStorage(md)
          val proofHash = ADProofs.proofDigest(proofBytes)
          if (fb.adProofs.isEmpty) onAdProofGenerated(ADProofs(fb.header.id, proofBytes))

            if (!store.get(Algos.idToBAW(fb.id)).exists(w => java.util.Arrays.equals(w.data, fb.header.stateRoot))) {
              throw new Exception("Storage kept roothash is not equal to the declared one")
            } else if (!java.util.Arrays.equals(fb.header.ADProofsRoot, proofHash)) {
              throw new Exception("Calculated proofHash is not equal to the declared one")
            } else if (!java.util.Arrays.equals(fb.header.stateRoot, persistentProver.digest)) {
              throw new Exception("Calculated stateRoot is not equal to the declared one")
            }

            log.info(s"Valid modifier with header ${fb.header.encodedId} and emission box " +
              s"${emissionBox.map(e => Algos.encode(e.id))} applied to UtxoState with root hash ${Algos.encode(inRoot)}")
            new UtxoState(persistentProver, idToVersion(fb.id), store, constants, settings)
          }
        stateTry.recoverWith[UtxoState] { case e =>
          log.warn(s"Error while applying full block with header ${fb.header.encodedId} to UTXOState with root" +
            s" ${Algos.encode(inRoot)}, reason: ${LoggingUtil.getReasonMsg(e)} ")
          persistentProver.rollback(inRoot).ensuring(java.util.Arrays.equals(persistentProver.digest, inRoot))
          Failure(e)
        }
      }
  }

  private def applyHeader: ModifierProcessing = {
    case h: Header =>
      Success(new UtxoState(persistentProver, idToVersion(h.id), this.store, constants, settings))
  }

  private def applySnapshot: ModifierProcessing = {
    case UtxoSnapshot(manifest, chunks, lastHeaders) =>
      val serializer = new BatchAVLProverSerializer[Digest32, HF]
      serializer.combine(manifest.proverManifest -> chunks.sortBy(_.index).flatMap(_.subtrees))
        .map { prover =>
          val recoveredStateContext = ErgoStateContext(lastHeaders, prover.digest)
          val newStore = dropStoreAndCreateNew()
          val recoveredPersistentProver = {
            val np = NodeParameters(keySize = Constants.HashLength, valueSize = None, labelSize = 32)
            val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(newStore, np)(Algos.hash)
            UtxoState.createPersistentProver(
              prover, storage, idToVersion(manifest.blockId), None, recoveredStateContext)
          }
          new UtxoState(recoveredPersistentProver, idToVersion(manifest.blockId), newStore, constants, settings)
        }
  }

  private def other: ModifierProcessing = {
    case a: Any =>
      log.info(s"Unhandled modifier: $a")
      Failure(new Exception("unknown modifier"))
  }

  private def dropStoreAndCreateNew(): LSMStore = {
    val stateDir = ErgoState.stateDir(settings)
    stateDir.mkdirs()
    stateDir.listFiles().foreach(_.delete())
    new LSMStore(stateDir, keepVersions = constants.keepVersions)
  }

  override def rollbackVersions: Iterable[VersionTag] = {
    persistentProver.synchronized {
      persistentProver.storage.rollbackVersions
        .map { v =>
          store.get(ByteArrayWrapper(Algos.hash(v))).map(w => bytesToVersion(w.data))
        }
        .collect {
          case Some(value) => value
        }
    }
  }

}

object UtxoState {

  type ModifierProcessing = PartialFunction[ErgoPersistentModifier, Try[UtxoState]]

  private lazy val bestVersionKey = Algos.hash("best state version")
  val EmissionBoxIdKey = Algos.hash("emission box id key")

  private def metadata(modId: VersionTag,
                       stateRoot: ADDigest,
                       currentEmissionBoxOpt: Option[ErgoBox],
                       context: ErgoStateContext): Seq[(Array[Byte], Array[Byte])] = {
    val modIdBytes = versionToBytes(modId)
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modIdBytes -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modIdBytes
    val bestVersion = bestVersionKey -> modIdBytes
    val eb = EmissionBoxIdKey -> currentEmissionBoxOpt.map(emissionBox => emissionBox.id).getOrElse(Array[Byte]())
    val cb = ErgoStateReader.ContextKey -> context.bytes

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion, eb, cb)
  }

  def create(dir: File, constants: StateConstants, settings: ErgoSettings): UtxoState = {
    val store = new LSMStore(dir, keepVersions = constants.keepVersions)
    val version = store.get(ByteArrayWrapper(bestVersionKey)).map(w => bytesToVersion(w.data))
      .getOrElse(ErgoState.genesisStateVersion)
    val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
      val bp = new BatchAVLProver[Digest32, HF](keyLength = Constants.HashLength, valueLengthOpt = None)
      val np = NodeParameters(keySize = Constants.HashLength, valueSize = None, labelSize = 32)
      val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(store, np)(Algos.hash)
      PersistentBatchAVLProver.create(bp, storage).get
    }
    new UtxoState(persistentProver, version, store, constants, settings)
  }

  def createPersistentProver(prover: BatchAVLProver[Digest32, HF],
                             storage: VersionedIODBAVLStorage[Digest32],
                             stateVersion: VersionTag,
                             currentEmissionBoxOpt: Option[ErgoBox],
                             stateContext: ErgoStateContext): PersistentBatchAVLProver[Digest32, HF] = {
    PersistentBatchAVLProver.create(
      prover,
      storage,
      metadata(stateVersion, prover.digest, currentEmissionBoxOpt, stateContext),
      paranoidChecks = true
    ).fold(e => throw new Exception(s"Failed to create PersistentProver: $e"), p => p)
  }

  def fromBoxHolder(bh: BoxHolder,
                    currentEmissionBoxOpt: Option[ErgoBox],
                    dir: File,
                    constants: StateConstants,
                    settings: ErgoSettings): UtxoState = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = Constants.HashLength, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val store = new LSMStore(dir, keepVersions = constants.keepVersions)
    val defaultStateContext = ErgoStateContext.empty(p.digest)
    val np = NodeParameters(keySize = Constants.HashLength, valueSize = None, labelSize = 32)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(store, np)(Algos.hash)
    val persistentProver =
      createPersistentProver(p, storage, ErgoState.genesisStateVersion, currentEmissionBoxOpt, defaultStateContext)

    new UtxoState(persistentProver, ErgoState.genesisStateVersion, store, constants, settings)
  }

}
