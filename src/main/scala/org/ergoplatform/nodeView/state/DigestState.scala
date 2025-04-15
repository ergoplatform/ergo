package org.ergoplatform.nodeView.state

import java.io.File
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.nodeView.state.ErgoState.ModifierProcessing
import org.ergoplatform.settings._
import org.ergoplatform.utils.LoggingUtil
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.db.{ByteArrayWrapper, LDBVersionedStore}
import org.ergoplatform.core._
import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
import org.ergoplatform.nodeView.LocallyGeneratedBlockSection
import org.ergoplatform.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest
import scorex.util.{ModifierId, ScorexLogging}

import scala.util.{Failure, Success, Try}

/**
  * Minimal state variant which is storing only digest of UTXO set authenticated as a dynamic dictionary.
  * See https://eprint.iacr.org/2016/994 for details on this mode.
  */
class DigestState protected(override val version: VersionTag,
                            override val rootDigest: ADDigest,
                            override val store: LDBVersionedStore,
                            override val ergoSettings: ErgoSettings)
  extends ErgoState[DigestState]
    with ScorexLogging {

  store.lastVersionID
    .foreach(id => require(version == bytesToVersion(id), "version should always be equal to store.lastVersionID"))

  private lazy val nodeSettings = ergoSettings.nodeSettings

  private[state] def validateTransactions(transactions: Seq[ErgoTransaction],
                                          expectedHash: ADDigest,
                                          proofs: ADProofs,
                                          currentStateContext: ErgoStateContext): Try[Unit] = {
    // Check modifications, returning sequence of old values
    val knownBoxesTry =
      ErgoState.stateChanges(transactions).map { stateChanges =>
        val boxesFromProofs: Seq[ErgoBox] =
          proofs.verify(stateChanges, rootDigest, expectedHash).get.map(v => ErgoBoxSerializer.parseBytes(v))
        (transactions.flatMap(_.outputs) ++ boxesFromProofs).map(o => (ByteArrayWrapper(o.id), o)).toMap
      }

    def checkBoxExistence(id: ErgoBox.BoxId): Try[ErgoBox] =
      knownBoxesTry.flatMap { knownBoxes =>
        knownBoxes
        .get(ByteArrayWrapper(id))
        .fold[Try[ErgoBox]](Failure(new Exception(s"Box with id ${Algos.encode(id)} not found")))(Success(_))
      }

    ErgoState.execTransactions(transactions, currentStateContext, nodeSettings)(checkBoxExistence)
      .toTry
      .map(_ => ())
  }

  def validate(mod: BlockSection): Try[Unit] = mod match {
    case fb: ErgoFullBlock =>
      fb.adProofs match {
        case None =>
          Failure(new Exception("Empty proofs when trying to apply full block to Digest state"))
        case Some(proofs) if !java.util.Arrays.equals(ADProofs.proofDigest(proofs.proofBytes), fb.header.ADProofsRoot) =>
          Failure(new Exception("Incorrect proofs digest"))
        case Some(proofs) =>
          stateContext.appendFullBlock(fb).flatMap { currentStateContext =>
            val txs = fb.blockTransactions.txs
            val declaredHash = fb.header.stateRoot
            validateTransactions(txs, declaredHash, proofs, currentStateContext)
          }
      }

    case _: Header => Success(Unit)

    case a: Any =>
      Failure(new Exception(s"Modifier not validated: $a"))
  }

  override def applyModifier(mod: BlockSection, estimatedTip: Option[Height])(generate: LocallyGeneratedBlockSection => Unit): Try[DigestState] =
    (processFullBlock orElse processHeader orElse processOther) (mod)

  @SuppressWarnings(Array("OptionGet"))
  override def rollbackTo(version: VersionTag): Try[DigestState] = {
    log.info(s"Rollback Digest State to version ${Algos.encode(version)}")
    val versionBytes = org.ergoplatform.core.versionToBytes(version)
    Try(store.rollbackTo(versionBytes)).map { _ =>
      store.clean(nodeSettings.keepVersions)
      val rootHash = ADDigest @@ store.get(versionBytes).get
      log.info(s"Rollback to version ${Algos.encode(version)} with roothash ${Algos.encoder.encode(rootHash)}")
      new DigestState(version, rootHash, store, ergoSettings)
    }
  }

  override def rollbackVersions: Iterable[VersionTag] = store.rollbackVersions().map(w => bytesToVersion(w))

  def close(): Unit = store.close()

  private def processFullBlock: ModifierProcessing[DigestState] = {
    case fb: ErgoFullBlock if nodeSettings.verifyTransactions =>
      log.info(s"Got new full block ${fb.encodedId} at height ${fb.header.height} with root " +
        s"${Algos.encode(fb.header.stateRoot)}. Our root is ${Algos.encode(rootDigest)}")
      validate(fb)
        .flatMap { _ =>
          val version: VersionTag = idToVersion(fb.header.id)
          stateContext.appendFullBlock(fb) match {
            case Success(sc) => update(version, fb.header.stateRoot, sc)
            case Failure(e) =>
              log.error(s"Can't modify state context due to ${e.getMessage} ", e)
              Failure(e)
          }
        }.recoverWith { case e =>
          log.warn(s"Invalid block ${fb.encodedId}, reason: ${LoggingUtil.getReasonMsg(e)}")
          Failure(e)
        }
  }

  private def processHeader: ModifierProcessing[DigestState] = {
    case h: Header =>
      log.info(s"Got new Header ${h.encodedId} with root ${Algos.encoder.encode(h.stateRoot)}")
      val version: VersionTag = idToVersion(h.id)
      stateContext.appendHeader(h) match {
        case Success(sc) => update(version, h.stateRoot, sc)
        case Failure(e) =>
          log.error(s"Can't modify state context due to ${e.getMessage} ", e)
          Failure(e)
      }
   }

  private def processOther: ModifierProcessing[DigestState] = {
    case other =>
      log.warn(s"Unhandled modifier: $other")
      Success(this)
  }

  private def update(newVersion: VersionTag,
                     newRootHash: ADDigest,
                     newStateContext: ErgoStateContext): Try[DigestState] = {

    val toUpdate = DigestState.metadata(newVersion, newRootHash, newStateContext)

    store.update(org.ergoplatform.core.versionToBytes(newVersion), Seq.empty, toUpdate).map { _ =>
      new DigestState(newVersion, newRootHash, store, ergoSettings)
    }
  }

  override def applyInputBlock(txs: InputBlockTransactionsData,
                               tempSetAdded: Array[ErgoBox],
                               tempSetRemoved: Seq[ModifierId]): Unit = ???

}

object DigestState extends ScorexLogging with ScorexEncoding {

  /**
    * Creates [[DigestState]] with provided `ErgoStateContext` instance corresponding to some version` and `rootHash`.
    */
  def recover(version: VersionTag,
              rootHash: ADDigest,
              stateContext: ErgoStateContext,
              dir: File,
              settings: ErgoSettings): Try[DigestState] = {
    val store = new LDBVersionedStore(dir, initialKeepVersions = settings.nodeSettings.keepVersions)
    val toUpdate = DigestState.metadata(version, rootHash, stateContext)

    store.update(org.ergoplatform.core.versionToBytes(version), Seq.empty, toUpdate).map { _ =>
      new DigestState(version, rootHash, store, settings)
    }
  }

  /**
    * Read digest state from disk, or generate it from genesis data if nothing on the disk
    */
  def create(versionOpt: Option[VersionTag],
             rootHashOpt: Option[ADDigest],
             dir: File,
             settings: ErgoSettings): DigestState = {
    val store = new LDBVersionedStore(dir, initialKeepVersions = settings.nodeSettings.keepVersions)
    Try {
      val context = ErgoStateReader.storageStateContext(store, settings)
      (versionOpt, rootHashOpt) match {
        case (Some(version), Some(rootHash)) =>
          val state = if (store.lastVersionID.map(w => bytesToVersion(w)).contains(version)) {
            new DigestState(version, rootHash, store, settings)
          } else {
            val inVersion = store.lastVersionID.map(w => bytesToVersion(w)).getOrElse(version)
            new DigestState(inVersion, rootHash, store, settings)
              .update(version, rootHash, context).get //sync store
          }
          state.ensuring(bytesToVersion(store.lastVersionID.get) == version)
        case (None, None) if store.lastVersionID.isEmpty =>
          ErgoState.generateGenesisDigestState(dir, settings)
        case _ =>
          val version = store.lastVersionID.get
          val rootHash = store.get(version).get
          new DigestState(bytesToVersion(version), ADDigest @@ rootHash, store, settings)
      }
    } match {
      case Success(state) => state
      case Failure(e) =>
        store.close()
        log.warn(s"Failed to create state with ${versionOpt.map(Algos.encode)} and ${rootHashOpt.map(encoder.encode)}", e)
        ErgoState.generateGenesisDigestState(dir, settings)
    }
  }

  protected def metadata(newVersion: VersionTag,
                         newRootHash: ADDigest,
                         newStateContext: ErgoStateContext): Seq[(Array[Byte], Array[Byte])] = Seq(
    org.ergoplatform.core.versionToBytes(newVersion) -> newRootHash,
    ErgoStateReader.ContextKey -> newStateContext.bytes
  )

}
