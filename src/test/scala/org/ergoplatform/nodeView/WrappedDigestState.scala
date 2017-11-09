package org.ergoplatform.nodeView

import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.settings.ErgoSettings

class WrappedDigestState(val digestState: DigestState, val wrappedUtxoState: WrappedUtxoState, val settings: ErgoSettings)
  extends DigestState(digestState.version, digestState.rootHash, digestState.store, settings.nodeSettings) {



}
