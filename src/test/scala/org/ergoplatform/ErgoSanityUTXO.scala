package org.ergoplatform

import org.ergoplatform.ErgoSanity.{HT, PM, UTXO_ST}
import org.ergoplatform.nodeView.WrappedUtxoState
import org.scalacheck.Gen

class ErgoSanityUTXO extends ErgoSanity[UTXO_ST] {
  override val historyGen: Gen[HT] = generateHistory(verifyTransactions = true, ADState = false, PoPoWBootstrap = false, -1)

  override val stateGen: Gen[WrappedUtxoState] = boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None))

  override def semanticallyValidModifier(state: UTXO_ST): PM = validFullBlock(None, state.asInstanceOf[WrappedUtxoState])

  override def semanticallyInvalidModifier(state: UTXO_ST): PM = invalidErgoFullBlockGen.sample.get
}
