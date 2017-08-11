package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import scorex.core.transaction.state.MinimalState.VersionTag
import ErgoState.Digest
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256Unsafe

import scala.util.{Failure, Success, Try}

/**
  * Minimal state variant which is storing only digest of UTXO authenticated as a dynamic dictionary.
  * See https://eprint.iacr.org/2016/994 for details on this mode.
  */
class DigestState(override val rootHash: Digest) extends ErgoState[DigestState] with ScorexLogging {

  implicit val hf = new Blake2b256Unsafe

  //todo: or hash(rootHash || headerHash)?
  override def version: VersionTag = rootHash

  override def validate(mod: ErgoPersistentModifier): Try[Unit] = mod match {
    case fb: ErgoFullBlock =>
      Try {
        assert(hf(fb.aDProofs.get.bytes).sameElements(fb.header.ADProofsRoot))

        val txs = fb.blockTransactions.txs
        val declaredHash = fb.header.stateRoot

        txs.foldLeft(Success(): Try[Unit]) { case (status, tx) =>
          status.flatMap(_ => tx.semanticValidity)
        }.flatMap(_ => fb.aDProofs.map(_.verify(boxChanges(txs), rootHash, declaredHash))
          .getOrElse(Failure(new Error("Proofs are empty"))))
      }.flatten
    case a: Any => log.info(s"Modifier not validated: $a"); Try(this)
  }

  //todo: utxo snapshot could go here
  override def applyModifier(mod: ErgoPersistentModifier): Try[DigestState] = mod match {
    case fb: ErgoFullBlock =>
      validate(fb).map(_ => new DigestState(fb.header.stateRoot))
    case a: Any => log.info(s"Unhandled modifier: $a"); Try(this)
  }

  override def rollbackTo(version: VersionTag): Try[DigestState] = Success(new DigestState(rootHash = version))
}
