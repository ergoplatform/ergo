package scorex.core.transaction.state

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.PersistentNodeViewModifier
import scala.util.Try


trait StateFeature

trait TransactionValidation extends StateFeature {
  def isValid(tx: ErgoTransaction): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[ErgoTransaction]): Seq[ErgoTransaction] = txs.filter(isValid)

  def validate(tx: ErgoTransaction): Try[Unit]
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}
