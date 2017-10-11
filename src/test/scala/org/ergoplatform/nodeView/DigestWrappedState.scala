package org.ergoplatform.nodeView

import org.ergoplatform.nodeView.state.DigestState

class DigestWrappedState(val digestState: DigestState, val wrappedUtxoState: WrappedUtxoState)
  extends DigestState(digestState.version, digestState.rootHash, wrappedUtxoState.store)
