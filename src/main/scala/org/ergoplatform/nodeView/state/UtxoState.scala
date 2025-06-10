package org.ergoplatform.nodeView.state

import java.io.File
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.core.VersionTag
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.ValidationRules.{fbDigestIncorrect, fbOperationFailed}
import org.ergoplatform.settings.{Algos, ErgoSettings, Parameters}
import org.ergoplatform.utils.LoggingUtil
import org.ergoplatform.core._
import org.ergoplatform.nodeView.LocallyGeneratedBlockSection
import org.ergoplatform.validation.ModifierValidator
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree}
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.crypto.hash.Digest32
import scorex.db.{ByteArrayWrapper, LDBVersionedStore}
import scorex.util.ModifierId

import scala.util.{Failure, Success, Try}

/**
  * Utxo set based state implementation
  *
  * @param persistentProver - persistent prover that builds authenticated AVL+ tree on top of utxo set
  * @param store            - storage of persistentProver that also keeps metadata
  * @param version          - current state version
  * @param ergoSettings     - protocol and client config to to get state-related settings from
  */
class UtxoState(override val persistentProver: PersistentBatchAVLProver[Digest32, HF],
                override val version: VersionTag,
                override val store: LDBVersionedStore,
                override protected val ergoSettings: ErgoSettings)
  extends ErgoState[UtxoState]
    with UtxoStateReader {

  import UtxoState.metadata

  override def rootDigest: ADDigest = persistentProver.synchronized {
    persistentProver.digest
  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = persistentProver.synchronized {
    val p = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encode(version)}")
    store.get(versionToBytes(version)) match {
      case Some(hash) =>
        val rootHash: ADDigest = ADDigest @@ hash
        val rollbackResult = p.rollback(rootHash).map { _ =>
          new UtxoState(p, version, store, ergoSettings)
        }
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encode(version)}"))
    }
  }

  /**
    *
    * @param transactions to be applied to state
    * @param headerId of the block these transactions belong to
    * @param expectedDigest AVL+ tree digest of UTXO set after applying operations from txs
    * @param currentStateContext Additional data required for transactions validation
    * @param softFieldsAllowed
    * @param checkUtxoSetTransformations
    * @return
    */
  private[state] def applyTransactions(transactions: Seq[ErgoTransaction],
                                       headerId: ModifierId,
                                       expectedDigest: ADDigest,
                                       currentStateContext: ErgoStateContext,
                                       softFieldsAllowed: Boolean = true,
                                       checkUtxoSetTransformations: Boolean = true): Try[Unit] = {
    val createdOutputs = transactions.flatMap(_.outputs).map(o => (ByteArrayWrapper(o.id), o)).toMap

    def checkBoxExistence(id: ErgoBox.BoxId): Try[ErgoBox] = createdOutputs
      .get(ByteArrayWrapper(id))
      .orElse(boxById(id))
      .fold[Try[ErgoBox]](Failure(new Exception(s"Box with id ${Algos.encode(id)} not found")))(Success(_))

    val txProcessing = ErgoState.execTransactions(transactions, currentStateContext, ergoSettings.nodeSettings, softFieldsAllowed)(checkBoxExistence)
    if (txProcessing.isValid && checkUtxoSetTransformations) {
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
        .validateNoFailure(fbOperationFailed, blockOpsTry, ErgoTransaction.modifierTypeId)
        .validateEquals(fbDigestIncorrect, expectedDigest, persistentProver.digest, headerId, Header.modifierTypeId)
        .result
        .toTry
    } else {
      txProcessing.toTry.map(_ => ())
    }
  }

  private def applyFullBlock(fb: ErgoFullBlock, estimatedTip: Option[Height])
                            (generate: LocallyGeneratedBlockSection => Unit): Try[UtxoState] = {
    val keepVersions = ergoSettings.nodeSettings.keepVersions

    // avoid storing versioned information in the database when block being processed is behind
    // blockchain tip by `keepVersions` blocks at least
    // we store `keepVersions` diffs in the database if chain tip is not known yet
    if (fb.height >= estimatedTip.getOrElse(0) - keepVersions) {
      if (store.getKeepVersions < keepVersions) {
        store.setKeepVersions(keepVersions)
      }
    } else {
      if (store.getKeepVersions > 0) {
        store.setKeepVersions(0)
      }
    }

    persistentProver.synchronized {
      val height = fb.header.height

      log.debug(s"Trying to apply full block with header ${fb.header.encodedId} at height $height")

      val inRoot = rootDigest

      val stateTry = stateContext.appendFullBlock(fb).flatMap { newStateContext =>
        val txsTry = applyTransactions(fb.blockTransactions.txs, fb.header.id, fb.header.stateRoot, newStateContext)

        txsTry.map { _: Unit =>
          val emissionBox = extractEmissionBox(fb)
          val meta = metadata(idToVersion(fb.id), fb.header.stateRoot, emissionBox, newStateContext)

          var proofBytes = persistentProver.generateProofAndUpdateStorage(meta)

          if (!store.get(org.ergoplatform.core.idToBytes(fb.id))
            .exists(w => java.util.Arrays.equals(w, fb.header.stateRoot))) {
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
            if (fb.height >= estimatedTip.getOrElse(Int.MaxValue) - ergoSettings.nodeSettings.adProofsSuffixLength) {
              val adProofs = ADProofs(fb.header.id, proofBytes)
              generate(LocallyGeneratedBlockSection(adProofs))
            }
          }

          log.info(s"Valid modifier with header ${fb.header.encodedId} and emission box " +
            s"${emissionBox.map(e => Algos.encode(e.id))} applied to UtxoState at height ${fb.header.height}")
          saveSnapshotIfNeeded(fb.height, estimatedTip)
          new UtxoState(persistentProver, idToVersion(fb.id), store, ergoSettings)
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
  }

  override def applyModifier(mod: BlockSection, estimatedTip: Option[Height])
                            (generate: LocallyGeneratedBlockSection => Unit): Try[UtxoState] = mod match {
    case fb: ErgoFullBlock => applyFullBlock(fb, estimatedTip)(generate)

    case bs: BlockSection =>
      log.warn(s"Only full-blocks are expected, found $bs")
      Success(this)
  }

  @SuppressWarnings(Array("OptionGet"))
  override def rollbackVersions: Iterable[VersionTag] = persistentProver.synchronized {
    persistentProver.storage.rollbackVersions.map { v =>
      bytesToVersion(store.get(Algos.hash(v)).get)
    }
  }

  override def applyInputBlock(txs: Seq[ErgoTransaction], previousTransactions: Seq[ErgoTransaction], header: Header): Try[Unit] = {
    // check transactions with class II transactions disabled and no UTXO set transformations checked and written
    val res = this.withTransactions(previousTransactions).applyTransactions(txs, header.id, header.stateRoot, stateContext,
                                softFieldsAllowed = false, checkUtxoSetTransformations = false)
    if (res.isFailure) {
      log.warn(s"Input block validation failed for ${header.id} : " + res)
    }
    val inputs = (txs ++ previousTransactions).flatMap(_.inputs).map(_.boxId) // todo: optimize
    if(inputs.size != inputs.distinct.size) {    // todo: optimize
      log.warn("Double spending")
     Failure[Unit](new Exception("Double spending"))
    } else {
      res
    }
  }

}

object UtxoState {

  /**
    * Short synonym for AVL+ tree type used in the node
    */
  type Manifest = BatchAVLProverManifest[Digest32]

  /**
    * Short synonym for AVL subtree type used in the node
    */
  type Subtree = BatchAVLProverSubtree[Digest32]


  /**
    * Manifest is associated with 32 bytes cryptographically strong unique id (root hash of the AVL tree under manifest)
    */
  type ManifestId = Digest32

  /**
    * Subtree is associated with 32 bytes cryptographically strong unique id (hash of subtree's root node)
    */
  type SubtreeId = Digest32

  private lazy val bestVersionKey = Algos.hash("best state version")
  val EmissionBoxIdKey: Digest32 = Algos.hash("emission box id key")


  /**
    * Block-specific metadata to write into database (in addition to AVL+ tree)
    *
    * @param modId - ID of a block (header) corresponding to UTXO set
    * @param stateRoot - UTXO set digest (hash and tree height) AFTER applying block `modId`
    * @param currentEmissionBoxOpt - current unspent emission script box
    * @param context - current state context used in input scripts validation (for the next block)
    *
    * @return binary-serialized metadata
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

  /**
    * @return UTXO set based state on top of existing database, or genesis state if the database is empty
    */
  def create(dir: File, settings: ErgoSettings): UtxoState = {
    val store = new LDBVersionedStore(dir, initialKeepVersions = settings.nodeSettings.keepVersions)
    val version = store.get(bestVersionKey).map(w => bytesToVersion(w))
      .getOrElse(ErgoState.genesisStateVersion)
    val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
      val bp = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
      val storage = new VersionedLDBAVLStorage(store)
      PersistentBatchAVLProver.create(bp, storage).get
    }
    new UtxoState(persistentProver, version, store, settings)
  }

  /**
    * Used in tests and to generate a genesis state.
    */
  @SuppressWarnings(Array("OptionGet", "TryGet"))
  def fromBoxHolder(bh: BoxHolder,
                    currentEmissionBoxOpt: Option[ErgoBox],
                    dir: File,
                    settings: ErgoSettings,
                    parameters: Parameters): UtxoState = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach { b =>
      p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess)
    }

    val store = new LDBVersionedStore(dir, initialKeepVersions = settings.nodeSettings.keepVersions)

    val defaultStateContext = ErgoStateContext.empty(settings.chainSettings, parameters)
    val storage = new VersionedLDBAVLStorage(store)
    val persistentProver = PersistentBatchAVLProver.create(
      p,
      storage,
      metadata(ErgoState.genesisStateVersion, p.digest, currentEmissionBoxOpt, defaultStateContext),
      paranoidChecks = true
    ).get

    new UtxoState(persistentProver, ErgoState.genesisStateVersion, store, settings)
  }

}
