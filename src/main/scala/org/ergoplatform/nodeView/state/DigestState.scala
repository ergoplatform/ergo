package org.ergoplatform.nodeView.state

import java.io.File

import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import scorex.core.transaction.state.MinimalState.VersionTag
import ErgoState.Digest
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.modifiers.history.ADProof
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

/**
  * Minimal state variant which is storing only digest of UTXO authenticated as a dynamic dictionary.
  * See https://eprint.iacr.org/2016/994 for details on this mode.
  */
class DigestState private (override val rootHash: Digest, store: Store) extends ErgoState[DigestState] with ScorexLogging {

  override def version: VersionTag = rootHash

  override def validate(mod: ErgoPersistentModifier): Try[Unit] = mod match {
    case fb: ErgoFullBlock =>
      Try {
        assert(ADProof.proofDigest(fb.aDProofs.get.proofBytes).sameElements(fb.header.ADProofsRoot))

        val txs = fb.blockTransactions.txs
        val declaredHash = fb.header.stateRoot

        txs.foldLeft(Success(): Try[Unit]) { case (status, tx) =>
          status.flatMap(_ => tx.semanticValidity)
        }.flatMap(_ => fb.aDProofs.map(_.verify(boxChanges(txs), rootHash, declaredHash))
          .getOrElse(Failure(new Error("Proofs are empty"))))
      }.flatten
    case a: Any => log.info(s"Modifier not validated: $a"); Try(this)
  }

  private def update(newVersion: Digest): Try[DigestState] = Try {
    store.update(ByteArrayWrapper(newVersion), Seq(), Seq())
    new DigestState(newVersion, store)
  }

  //todo: utxo snapshot could go here
  override def applyModifier(mod: ErgoPersistentModifier): Try[DigestState] = mod match {
    case fb: ErgoFullBlock => validate(fb).flatMap(_ => update(fb.header.stateRoot))

      //todo: fail here? or not?
    case a: Any => log.info(s"Unhandled modifier: $a"); Try(this)
  }

  override def rollbackTo(version: VersionTag): Try[DigestState] = {
    Try(store.rollback(ByteArrayWrapper(version))).map(_ => new DigestState(rootHash = version, store))
  }

  override def rollbackVersions: Iterable[Digest] = store.rollbackVersions().map(_.data)
}

object DigestState {
  def create(rootHashOpt: Option[Digest], dir: File): Try[DigestState] = Try {
    val store = new LSMStore(dir, keepVersions = 10)

    rootHashOpt match {

      case Some(rootHash) =>
        if (store.lastVersionID.isDefined) {
          new DigestState(rootHash, store)
        } else {
          new DigestState(rootHash, store).update(rootHash).get //sync store
        }.ensuring(store.lastVersionID.get.data.sameElements(rootHash))

      case None =>
        val rootHash = store.lastVersionID.get.data
        new DigestState(rootHash, store)
    }
  }

  def create(rootHash: Digest, dir: File): Try[DigestState] = create(Some(rootHash), dir)
}