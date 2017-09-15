package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import scorex.core.VersionTag
import scorex.core.transaction.state.ModifierValidation
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base16

import scala.util.{Failure, Success, Try}

/**
  * Minimal state variant which is storing only digest of UTXO authenticated as a dynamic dictionary.
  * See https://eprint.iacr.org/2016/994 for details on this mode.
  */
class DigestState private(override val version: VersionTag, override val rootHash: ADDigest, store: Store)
  extends ErgoState[DigestState]
    with ModifierValidation[ErgoPersistentModifier]
    with ScorexLogging {

  def validate(mod: ErgoPersistentModifier): Try[Unit] = mod match {
    case fb: ErgoFullBlock =>
      Try {
        assert(ADProofs.proofDigest(fb.aDProofs.get.proofBytes).sameElements(fb.header.ADProofsRoot))

        val txs = fb.blockTransactions.txs
        val declaredHash = fb.header.stateRoot

        txs.foldLeft(Success(): Try[Unit]) { case (status, tx) =>
          status.flatMap(_ => tx.semanticValidity)
        }.flatMap(_ => fb.aDProofs.map(_.verify(boxChanges(txs), rootHash, declaredHash))
          .getOrElse(Failure(new Error("Proofs are empty"))))
      }.flatten match {
        case s: Success[_] =>
          log.info(s"Valid modifier applied to DigestState: ${Base16.encode(mod.id)}")
          s
        case Failure(e) =>
          log.warn(s"Modifier $mod is not valid: ", e)
          Failure(e)
      }
    case a: Any => log.info(s"Modifier not validated: $a"); Try(this)
  }

  private def update(newVersion: VersionTag, newRootHash: ADDigest): Try[DigestState] = Try {
    val wrappedVersion = ByteArrayWrapper(newVersion)
    store.update(wrappedVersion, toRemove = Seq(), toUpdate = Seq(wrappedVersion -> ByteArrayWrapper(rootHash)))
    new DigestState(newVersion, newRootHash, store)
  }

  //todo: utxo snapshot could go here
  override def applyModifier(mod: ErgoPersistentModifier): Try[DigestState] = mod match {
    case fb: ErgoFullBlock => this.validate(fb).flatMap(_ => update(VersionTag @@ fb.id, fb.header.stateRoot))

    //todo: fail here? or not?
    case a: Any => log.info(s"Unhandled modifier: $a"); Try(this)
  }

  override def rollbackTo(version: VersionTag): Try[DigestState] = {
    val wrappedVersion = ByteArrayWrapper(version)
    Try(store.rollback(wrappedVersion)).map { _ =>
      val rootHash = ADDigest @@ store.get(wrappedVersion).get.data
      new DigestState(version, rootHash, store)
    }
  }

  override def rollbackVersions: Iterable[VersionTag] = store.rollbackVersions().map(VersionTag @@ _.data)
}

object DigestState {

  //todo: rework, this method is very wrong
  def create(version: VersionTag, rootHashOpt: Option[ADDigest], dir: File): Try[DigestState] = Try {
    val store = new LSMStore(dir, keepVersions = 10)

    rootHashOpt match {

      case Some(rootHash) =>
        if (store.lastVersionID.isDefined) {
          new DigestState(version, rootHash, store)
        } else {
          new DigestState(version, rootHash, store).update(version, rootHash).get //sync store
        }.ensuring(store.lastVersionID.get.data.sameElements(version))

      case None =>
        val rootHash = ADDigest @@ store.get(store.lastVersionID.get).get.data

        new DigestState(version, rootHash, store)
    }
  }

  def create(rootHash: ADDigest, dir: File): Try[DigestState] = create(ErgoState.genesisStateVersion, Some(rootHash), dir)
}