package org.ergoplatform.nodeView.state

import java.io.File
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.ValidationRules.{fbDigestIncorrect, fbOperationFailed}
import org.ergoplatform.settings.{Algos, Parameters}
import org.ergoplatform.utils.LoggingUtil
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core._
import scorex.core.transaction.Transaction
import scorex.core.transaction.state.TransactionValidation
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree}
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.crypto.hash.Digest32
import scorex.db.{ByteArrayWrapper, LDBVersionedStore}
import scorex.util.ModifierId
import scorex.util.ScorexLogging

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
                override val store: LDBVersionedStore,
                override val constants: StateConstants)
  extends ErgoState[UtxoState]
    with TransactionValidation
    with UtxoStateReader
    with ScorexEncoding {

  import UtxoState.metadata

  override def rootHash: ADDigest = persistentProver.synchronized {
    persistentProver.digest
  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = persistentProver.synchronized {
    val p = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    store.get(scorex.core.versionToBytes(version)) match {
      case Some(hash) =>
        val rootHash: ADDigest = ADDigest @@ hash
        val rollbackResult = p.rollback(rootHash).map { _ =>
          new UtxoState(p, version, store, constants)
        }
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  /**
    *
    * @param transactions to be applied to state
    * @param headerId of the block these transactions belong to
    * @param expectedDigest AVL+ tree digest of UTXO set after applying operations from txs
    * @param currentStateContext Additional data required for transactions validation
    * @return
    */
  private[state] def applyTransactions(transactions: Seq[ErgoTransaction],
                                       headerId: ModifierId,
                                       expectedDigest: ADDigest,
                                       currentStateContext: ErgoStateContext): Try[Unit] = {
    val createdOutputs = transactions.flatMap(_.outputs).map(o => (ByteArrayWrapper(o.id), o)).toMap

    def checkBoxExistence(id: ErgoBox.BoxId): Try[ErgoBox] = createdOutputs
      .get(ByteArrayWrapper(id))
      .orElse(boxById(id))
      .fold[Try[ErgoBox]](Failure(new Exception(s"Box with id ${Algos.encode(id)} not found")))(Success(_))

    val txProcessing = ErgoState.execTransactions(transactions, currentStateContext)(checkBoxExistence)
    if (txProcessing.isValid) {
      log.debug(s"Cost of block $headerId (${currentStateContext.currentHeight}): ${txProcessing.payload.getOrElse(0)}")
      val blockOpsTry = ErgoState.stateChanges(transactions).flatMap { stateChanges =>
        val operations = stateChanges.operations
        var opsResult: Try[Unit] = Success(())
        operations.foreach { op =>
          if (opsResult.isSuccess) {
            persistentProver.performOneOperation(op) match {
              case Success(_) =>
              case Failure(t) =>
                log.error(s"Operation $op failed during $headerId transactions validation")
                opsResult = Failure(t)
            }
          }
        }
        opsResult
      }
      ModifierValidator(stateContext.validationSettings)
        .validateNoFailure(fbOperationFailed, blockOpsTry, Transaction.ModifierTypeId)
        .validateEquals(fbDigestIncorrect, expectedDigest, persistentProver.digest, headerId, Header.modifierTypeId)
        .result
        .toTry
    } else {
      txProcessing.toTry.map(_ => ())
    }
  }

  override def applyModifier(mod: BlockSection, estimatedTip: Option[Height])
                            (generate: LocallyGeneratedModifier => Unit): Try[UtxoState] = mod match {
    case fb: ErgoFullBlock =>

      // avoid storing versioned information in the database when block being processed is behind
      // blockchain tip by `keepVersions` blocks at least
      // we store `keepVersions` diffs in the database if chain tip is not known yet
      if (fb.height >= estimatedTip.getOrElse(0) - constants.keepVersions) {
        if (store.getKeepVersions < constants.keepVersions) {
          store.setKeepVersions(constants.keepVersions)
        }
      } else {
        if (store.getKeepVersions > 0) {
          store.setKeepVersions(0)
        }
      }

      persistentProver.synchronized {
        val height = fb.header.height

        log.debug(s"Trying to apply full block with header ${fb.header.encodedId} at height $height")

        val inRoot = rootHash

        val stateTry = stateContext.appendFullBlock(fb).flatMap { newStateContext =>
          val txsTry = applyTransactions(fb.blockTransactions.txs, fb.header.id, fb.header.stateRoot, newStateContext)

          txsTry.map { _: Unit =>
            val emissionBox = extractEmissionBox(fb)
            val meta = metadata(idToVersion(fb.id), fb.header.stateRoot, emissionBox, newStateContext)

            var proofBytes = persistentProver.generateProofAndUpdateStorage(meta)

            if (!store.get(scorex.core.idToBytes(fb.id)).exists(w => java.util.Arrays.equals(w, fb.header.stateRoot))) {
              throw new Exception("Storage kept roothash is not equal to the declared one")
            }

            if (!java.util.Arrays.equals(fb.header.stateRoot, persistentProver.digest)) {
              throw new Exception("Calculated stateRoot is not equal to the declared one")
            }

            var proofHash = ADProofs.proofDigest(proofBytes)

            if (!java.util.Arrays.equals(fb.header.ADProofsRoot, proofHash)) {

              log.error("Calculated proofHash is not equal to the declared one, doing another attempt")

              /**
                * Proof generated was different from one announced.
                *
                * In most cases, announced proof is okay, and as proof is already checked, problem in some
                * extra bytes added to the proof.
                *
                * Could be related to https://github.com/ergoplatform/ergo/issues/1614
                *
                * So the problem could appear on mining nodes only, and caused by
                * proofsForTransactions() wasting the tree unexpectedly.
                *
                * We are trying to generate proof again now.
                */

              persistentProver.rollback(inRoot)
                .ensuring(java.util.Arrays.equals(persistentProver.digest, inRoot))

              ErgoState.stateChanges(fb.blockTransactions.txs) match {
                case Success(stateChanges) =>
                 val mods = stateChanges.operations
                  mods.foreach( modOp => persistentProver.performOneOperation(modOp))

                  // meta is the same as it is block-specific
                  proofBytes = persistentProver.generateProofAndUpdateStorage(meta)
                  proofHash = ADProofs.proofDigest(proofBytes)

                  if(!java.util.Arrays.equals(fb.header.ADProofsRoot, proofHash)) {
                    throw new Exception("Regenerated proofHash is not equal to the declared one")
                  }
                case Failure(e) =>
                  throw new Exception("Can't generate state changes on proof regeneration ", e)
              }
            }

            if (fb.adProofs.isEmpty) {
              if (fb.height >= estimatedTip.getOrElse(Int.MaxValue) - stateContext.ergoSettings.nodeSettings.adProofsSuffixLength) {
                val adProofs = ADProofs(fb.header.id, proofBytes)
                generate(LocallyGeneratedModifier(adProofs))
              }
            }

            log.info(s"Valid modifier with header ${fb.header.encodedId} and emission box " +
              s"${emissionBox.map(e => Algos.encode(e.id))} applied to UtxoState at height ${fb.header.height}")
            saveSnapshotIfNeeded(fb.height, estimatedTip)
            new UtxoState(persistentProver, idToVersion(fb.id), store, constants)
          }
        }
        stateTry.recoverWith[UtxoState] { case e =>
          log.warn(s"Error while applying full block with header ${fb.header.encodedId} to UTXOState with root" +
            s" ${Algos.encode(inRoot)}, reason: ${LoggingUtil.getReasonMsg(e)} ", e)
          persistentProver.rollback(inRoot)
            .ensuring(java.util.Arrays.equals(persistentProver.digest, inRoot))
          Failure(e)
        }
      }

    case h: Header =>
      log.warn("Only full-blocks are expected (before UTXO snapshot downloading implementation")
      //todo: update state context with headers (when snapshot downloading is done), so
      //todo: application of the first full block after the snapshot should have correct state context
      //todo: (in particular, "lastHeaders" field of it)
      Success(new UtxoState(persistentProver, idToVersion(h.id), this.store, constants))

    case a: Any =>
      log.error(s"Unhandled unknown modifier: $a")
      Failure(new Exception("unknown modifier"))
  }

  @SuppressWarnings(Array("OptionGet"))
  override def rollbackVersions: Iterable[VersionTag] = persistentProver.synchronized {
    persistentProver.storage.rollbackVersions.map { v =>
      bytesToVersion(store.get(Algos.hash(v)).get)
    }
  }



}

object UtxoState extends ScorexLogging {

  type Manifest = BatchAVLProverManifest[Digest32]
  type Subtree = BatchAVLProverSubtree[Digest32]

  type ManifestId = Digest32
  type SubtreeId = Digest32

  private lazy val bestVersionKey = Algos.hash("best state version")
  val EmissionBoxIdKey: Digest32 = Algos.hash("emission box id key")

  // block-specific metadata to write into database (in addition to AVL+ tree)

  /**
    *
    * @param modId - ID of a block (header) corresponding to UTXO set
    * @param stateRoot - UTXO set digest (hash and tree height) AFTER applying block `modId`
    * @param currentEmissionBoxOpt
    * @param context
    * @return
    */
  def metadata(modId: VersionTag,
               stateRoot: ADDigest,
               currentEmissionBoxOpt: Option[ErgoBox],
               context: ErgoStateContext): Seq[(Array[Byte], Array[Byte])] = {
    val modIdBytes = versionToBytes(modId)
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modIdBytes -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modIdBytes
    val bestVersion = bestVersionKey -> modIdBytes
    val eb = EmissionBoxIdKey -> currentEmissionBoxOpt.map(emissionBox => emissionBox.id).getOrElse(Array[Byte]())
    val cb = ErgoStateReader.ContextKey -> context.bytes

    Array(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion, eb, cb)
  }

  def create(dir: File, constants: StateConstants): UtxoState = {
    val store = new LDBVersionedStore(dir, initialKeepVersions = constants.keepVersions)
    val version = store.get(bestVersionKey).map(w => bytesToVersion(w))
      .getOrElse(ErgoState.genesisStateVersion)
    val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
      val bp = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
      val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedLDBAVLStorage[Digest32, HF] = new VersionedLDBAVLStorage(store, np)(Algos.hash)
      PersistentBatchAVLProver.create(bp, storage).get
    }
    new UtxoState(persistentProver, version, store, constants)
  }

  /**
    * Used in tests and to generate a genesis state.
    */
  @SuppressWarnings(Array("OptionGet", "TryGet"))
  def fromBoxHolder(bh: BoxHolder,
                    currentEmissionBoxOpt: Option[ErgoBox],
                    dir: File,
                    constants: StateConstants,
                    parameters: Parameters): UtxoState = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach { b =>
      p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess)
    }

    val store = new LDBVersionedStore(dir, initialKeepVersions = constants.keepVersions)

    val defaultStateContext = ErgoStateContext.empty(constants, parameters)
    val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
    val storage: VersionedLDBAVLStorage[Digest32, HF] = new VersionedLDBAVLStorage(store, np)(Algos.hash)
    val persistentProver = PersistentBatchAVLProver.create(
      p,
      storage,
      metadata(ErgoState.genesisStateVersion, p.digest, currentEmissionBoxOpt, defaultStateContext),
      paranoidChecks = true
    ).get

    new UtxoState(persistentProver, ErgoState.genesisStateVersion, store, constants)
  }

}
