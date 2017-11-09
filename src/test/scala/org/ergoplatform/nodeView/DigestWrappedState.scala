package org.ergoplatform.nodeView

import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.settings.ErgoSettings

class DigestWrappedState(val digestState: DigestState, val wrappedUtxoState: WrappedUtxoState, val settings: ErgoSettings)
  extends DigestState(digestState.version, digestState.rootHash, wrappedUtxoState.store, settings.nodeSettings)
