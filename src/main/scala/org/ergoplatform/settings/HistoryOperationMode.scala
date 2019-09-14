package org.ergoplatform.settings

sealed trait HistoryOperationMode

/**
  * ADT describing possible history component operation modes.
  */
object HistoryOperationMode {
  // Full chain
  case object Full extends HistoryOperationMode
  // Full chain + PoPow prover mode
  case object FullProving extends HistoryOperationMode
  // Full PoPow bootstrapped chain
  case object FullPoPow extends HistoryOperationMode
  // Headers chain
  case object Light extends HistoryOperationMode
  // PoPow bootstrapped headers chain
  case object LightPoPow extends HistoryOperationMode
}
