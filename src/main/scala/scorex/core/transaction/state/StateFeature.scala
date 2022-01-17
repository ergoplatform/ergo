package scorex.core.transaction.state

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scala.util.Try

/**
  * Basic trait for features supported by state representation
  */
trait StateFeature

/**
  * Instance of this trait supports stateful validation of any transaction
  */
trait TransactionValidation extends StateFeature {
  def validateWithCost(tx: ErgoTransaction, maxTxCost: Long): Try[Long]
}

object TransactionValidation {
  case class TooHighCostError(message: String) extends Exception(message)
}

