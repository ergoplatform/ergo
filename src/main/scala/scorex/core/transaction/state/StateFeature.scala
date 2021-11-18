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

object TransactionValidation {
  case class TooHighComplexityError(message: String) extends Exception(message)
  case class TooHighCostError(message: String) extends Exception(message)
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}
