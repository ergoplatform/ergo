package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.{ErgoStateContext, ErgoBoxSerializer}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{Algos, Constants, NodeConfigurationSettings}
import scorex.core.VersionTag
import scorex.core.transaction.state.ModifierValidation
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLVerifier, Insert, Remove}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

/**
  * Minimal state variant which is storing only digest of UTXO authenticated as a dynamic dictionary.
  * See https://eprint.iacr.org/2016/994 for details on this mode.
  */
class DigestState protected(override val version: VersionTag,
                            override val rootHash: ADDigest,
                            override val store: Store,
                            settings: NodeConfigurationSettings)
  extends ErgoState[DigestState]
    with ModifierValidation[ErgoPersistentModifier]
    with ScorexLogging {

  store.lastVersionID
    .foreach(id => assert(version sameElements id.data, "version should always be equal to store.lastVersionID"))

  override lazy val maxRollbackDepth: Int = store.rollbackVersions().size

  def validate(mod: ErgoPersistentModifier): Try[Unit] = mod match {
    case fb: ErgoFullBlock if notInitialized => Success()

    case fb: ErgoFullBlock =>
      fb.aDProofs match {
        case Some(proofs) if !ADProofs.proofDigest(proofs.proofBytes).sameElements(fb.header.ADProofsRoot) =>
          Failure(new Error("Incorrect proofs digest"))
        case Some(proofs) =>
          Try {
            val blockchainState = ErgoStateContext(fb.header.height, rootHash)

            val txs = fb.blockTransactions.txs

            val maxOps = txs.map(tx => tx.inputs.size + tx.outputCandidates.size).sum

            val verifier = new BatchAVLVerifier[Digest32, HF](rootHash, proofs.proofBytes, ADProofs.KL,
              None, maxNumOperations = Some(maxOps))

            val declaredHash = fb.header.stateRoot
            // Check modifications, returning sequence of old values
            val oldValues: Seq[ErgoBox] = proofs.verify(ErgoState.boxChanges(txs), rootHash, declaredHash)
              .get.map(v => ErgoBoxSerializer.parseBytes(v).get)
            val knownBoxes = (txs.flatMap(_.outputs) ++ oldValues).map(o => (ByteArrayWrapper(o.id), o)).toMap
            val totalCost = txs.map { tx =>
              tx.statelessValidity.get
              val boxesToSpend = tx.inputs.map(_.boxId).map { id =>
                knownBoxes.get(ByteArrayWrapper(id)) match {
                  case Some(box) => box
                  case None => throw new Error(s"Box with id ${Algos.encode(id)} not found")
                }
              }
              tx.statefulValidity(boxesToSpend, blockchainState).get
            }.sum
            if (totalCost > Constants.MaxTransactionCost) throw new Error(s"Transaction cost $totalCost exeeds limit")

          }
        case None =>
          Failure(new Error("Empty proofs when trying to apply full block to Digest state"))
      }

    case _: Header => Success()

    case a: Any =>
      Failure(new Error(s"Modifier not validated: $a"))
  }

  //todo: utxo snapshot could go here
  override def applyModifier(mod: ErgoPersistentModifier): Try[DigestState] = mod match {
    case fb: ErgoFullBlock if settings.verifyTransactions =>
      log.info(s"Got new full block ${fb.encodedId} at height ${fb.header.height} with root " +
        s"${Algos.encode(fb.header.stateRoot)}. Our root is ${Algos.encode(rootHash)}")
      this.validate(fb).flatMap(_ => update(VersionTag @@ fb.header.id, fb.header.stateRoot)).recoverWith {
        case e =>
          log.warn(s"Invalid block ${fb.encodedId}, reason: ", e)
          Failure(e)
      }

    case fb: ErgoFullBlock if !settings.verifyTransactions =>
      //TODO should not get this messages from node view holders
      Try(this)

    case h: Header if !settings.verifyTransactions =>
      log.info(s"Got new Header ${h.encodedId} with root ${Algos.encoder.encode(h.stateRoot)}")
      update(VersionTag @@ h.id, h.stateRoot)

    case h: Header if settings.verifyTransactions =>
      //TODO should not get this messages from node view holders
      Try(this)

    case a: Any =>
      //todo: fail here? or not?
      log.info(s"Unhandled modifier: $a")
      Try(this)
  }

  @SuppressWarnings(Array("OptionGet"))
  override def rollbackTo(version: VersionTag): Try[DigestState] = {
    log.info(s"Rollback Digest State to version ${Algos.encoder.encode(version)}")
    val wrappedVersion = ByteArrayWrapper(version)
    Try(store.rollback(wrappedVersion)).map { _ =>
      store.clean(ErgoState.KeepVersions)
      val rootHash = ADDigest @@ store.get(wrappedVersion).get.data
      log.info(s"Rollback to version ${Algos.encoder.encode(version)} with roothash ${Algos.encoder.encode(rootHash)}")
      new DigestState(version, rootHash, store, settings)
    }
  }

  override def rollbackVersions: Iterable[VersionTag] = store.rollbackVersions().map(VersionTag @@ _.data)

  def close(): Unit = store.close()

  private def update(newVersion: VersionTag, newRootHash: ADDigest): Try[DigestState] = Try {
    val wrappedVersion = ByteArrayWrapper(newVersion)
    store.update(wrappedVersion, toRemove = Seq.empty, toUpdate = Seq(wrappedVersion -> ByteArrayWrapper(newRootHash)))
    new DigestState(newVersion, newRootHash, store, settings)
  }

  // DigestState is not initialized yet. Waiting for first full block to apply without checks
  private lazy val notInitialized = settings.blocksToKeep >= 0 && (version sameElements ErgoState.genesisStateVersion)

}

object DigestState {

  def create(versionOpt: Option[VersionTag],
             rootHashOpt: Option[ADDigest],
             dir: File,
             settings: NodeConfigurationSettings): DigestState = Try {
    val store = new LSMStore(dir, keepVersions = ErgoState.KeepVersions) //todo: read from settings

    (versionOpt, rootHashOpt) match {
      case (Some(version), Some(rootHash)) =>
        val state = if (store.lastVersionID.isDefined && store.lastVersionID.forall(_.data sameElements version)) {
          new DigestState(version, rootHash, store, settings)
        } else {
          val inVersion = VersionTag @@ store.lastVersionID.map(_.data).getOrElse(version)
          new DigestState(inVersion, rootHash, store, settings).update(version, rootHash).get //sync store
        }
        state.ensuring(store.lastVersionID.get.data.sameElements(version))
      case (None, None) =>
        val version = VersionTag @@ store.lastVersionID.get.data
        val rootHash = store.get(ByteArrayWrapper(version)).get.data
        new DigestState(version, ADDigest @@ rootHash, store, settings)
      case _ => ???
    }
  }.getOrElse(ErgoState.generateGenesisDigestState(dir, settings))
}
