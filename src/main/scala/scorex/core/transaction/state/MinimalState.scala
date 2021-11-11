package scorex.core.transaction.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoStateReader
import scorex.core.{PersistentNodeViewModifier, VersionTag}

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait MinimalState[MS <: MinimalState[MS]] extends ErgoStateReader {
  self: MS =>

  def applyModifier(mod: ErgoPersistentModifier): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]

}


trait StateFeature

trait TransactionValidation extends StateFeature {
  def isValid(tx: ErgoTransaction): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[ErgoTransaction]): Seq[ErgoTransaction] = txs.filter(isValid)

  def validate(tx: ErgoTransaction): Try[Unit]
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}
