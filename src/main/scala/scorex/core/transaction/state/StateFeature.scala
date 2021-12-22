package scorex.core.transaction.state

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.PersistentNodeViewModifier
import scala.util.Try


trait StateFeature

trait TransactionValidation extends StateFeature {
  def validate(tx: ErgoTransaction): Try[Unit]
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}
