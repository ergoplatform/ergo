package org.ergoplatform.settings

sealed trait HistoryOperationMode

/**
  * ADT describing history component operation modes.
  */
object HistoryOperationMode {
  // Full chain
  object Full extends HistoryOperationMode
  // Full chain + PoPow prover mode
  object FullProving extends HistoryOperationMode
  // Full PoPow bootstrapped chain
  object FullPoPow extends HistoryOperationMode
  // Headers chain
  object Light extends HistoryOperationMode
  // Headers chain
  object LightPoPow extends HistoryOperationMode
}
