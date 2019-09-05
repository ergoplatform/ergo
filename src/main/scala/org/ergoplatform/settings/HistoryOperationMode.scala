package org.ergoplatform.settings

sealed trait HistoryOperationMode

object HistoryOperationMode {
  // Full chain
  object Full extends HistoryOperationMode
  // Full chain + PoPow prover mode
  object FullProving extends HistoryOperationMode
  // Headers chain
  object Light extends HistoryOperationMode
  // Full PoPow bootstrapped chain
  object FullPoPow extends HistoryOperationMode
  // PoPow bootstrapped headers chain
  object LightPoPow extends HistoryOperationMode
}
