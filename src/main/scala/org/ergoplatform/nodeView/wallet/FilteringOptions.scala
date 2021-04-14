package org.ergoplatform.nodeView.wallet

/**
  * Request to filter transactions by height or by confirmations
  */
case class FilteringOptions(minHeight: Int, maxHeight: Int, minConfNum: Int, maxConfNum: Int)
