package org.ergoplatform

import org.ergoplatform.ErgoSanity.{DIGEST_ST, HT, PM}
import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.nodeView.{DigestWrappedState, WrappedUtxoState}
import org.scalacheck.Gen

class ErgoSanityDigest extends ErgoSanity[DIGEST_ST] {
  override val historyGen: Gen[HT] = generateHistory(verifyTransactions = true, ADState = true, PoPoWBootstrap = false, -1)

  override val stateGen: Gen[DigestWrappedState] = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir)).map{ wus =>
      new DigestWrappedState(DigestState.create(wus.rootHash, createTempDir).get, wus)
    }
  }

  override def semanticallyValidModifier(state: DIGEST_ST): PM = validFullBlock(None, state.asInstanceOf[DigestWrappedState].wrappedUtxoState)

  override def semanticallyInvalidModifier(state: DIGEST_ST): PM = invalidErgoFullBlockGen.sample.get
}
