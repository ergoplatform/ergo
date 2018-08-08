package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox
import sigmastate.SType
import sigmastate.Values.EvaluatedValue

/**
  * A payment request contains a sequence of (script, value, assets, additional registers) tuples.
  */
case class PaymentRequest(to: (ErgoAddress, Long, Map[ErgoBox.TokenId, Long], Seq[EvaluatedValue[_ <: SType]])*)
