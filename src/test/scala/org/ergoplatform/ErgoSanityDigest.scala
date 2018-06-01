package org.ergoplatform

import org.ergoplatform.ErgoSanity.{DIGEST_ST, HT, PM}
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.nodeView.{WrappedDigestState, WrappedUtxoState}
import org.scalacheck.Gen

class ErgoSanityDigest extends ErgoSanity[DIGEST_ST] {
  override val historyGen: Gen[HT] = generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, -1)

  override val stateGen: Gen[WrappedDigestState] = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, emission, None)).map { wus =>
      val digestState = DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, settings)
      new WrappedDigestState(digestState, wus, settings)
    }
  }

  override def semanticallyValidModifier(state: DIGEST_ST): PM = validFullBlock(None, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)

  override def semanticallyInvalidModifier(state: DIGEST_ST): PM = invalidErgoFullBlockGen.sample.get

}
