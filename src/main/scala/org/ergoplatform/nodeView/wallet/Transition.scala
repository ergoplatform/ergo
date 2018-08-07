package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height

/**
  * Possible reasons of a tracked box state change:
  * - a transaction produced the box got confirmed
  * - a transaction spending the box (confirmed or not) appeared or got confirmed (we process both events in a handler)
  * - rollback
  */

sealed trait Transition

case class CreationConfirmation(creationHeight: Height) extends Transition
case class ProcessSpending(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]) extends Transition
case class ProcessRollback(toHeight: Int) extends Transition

