package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height

sealed trait Transition

case class ProcessRollback(toHeight: Int) extends Transition
case class CreationConfirmation(creationHeight: Height) extends Transition
case class ProcessSpending(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]) extends Transition

