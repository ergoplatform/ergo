package org.ergoplatform.settings

/**
  * ADT describing all possible history component operation modes.
  */
sealed trait HistoryOperationMode

object HistoryOperationMode {
  // Full chain
  case object Full extends HistoryOperationMode
  // Full chain + PoPow prover mode
  case object FullProving extends HistoryOperationMode
  // Headers chain
  case object Light extends HistoryOperationMode
}
