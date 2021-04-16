package org.ergoplatform.nodeView.wallet

/**
  * Request to filter transactions by height or by confirmations
  */

sealed trait WalletFiltering

case class ByHeight(minHeight: Int, maxHeight: Int) extends WalletFiltering

case class ByConfirmationsNum(minConfNum: Int, maxConfNum: Int) extends WalletFiltering
