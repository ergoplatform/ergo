package scorex.core.transaction

import org.ergoplatform.modifiers.mempool.ErgoTransaction

/**
  * Exception which is indicating that transaction had too high cost during validation
  */
case class TooHighCostError(tx: ErgoTransaction, txCost: Option[Int])
  extends Exception(s"Transaction $tx has too high cost ${txCost.map(_.toString).getOrElse("")}")
