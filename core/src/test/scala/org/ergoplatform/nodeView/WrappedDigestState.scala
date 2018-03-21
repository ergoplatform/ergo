package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.settings.ErgoSettings
import scorex.core.VersionTag

import scala.util.Try

class WrappedDigestState(val digestState: DigestState, val wrappedUtxoState: WrappedUtxoState, val settings: ErgoSettings)
  extends DigestState(digestState.version, digestState.rootHash, digestState.store, settings.nodeSettings) {

  override def applyModifier(mod: ErgoPersistentModifier): Try[WrappedDigestState] = {
    wrapped(super.applyModifier(mod), wrappedUtxoState.applyModifier(mod))
  }

  override def rollbackTo(version: VersionTag): Try[WrappedDigestState] = {
    wrapped(super.rollbackTo(version), wrappedUtxoState.rollbackTo(version))
  }

  private def wrapped(digestT: Try[DigestState], utxoT: Try[WrappedUtxoState]): Try[WrappedDigestState] =
    digestT.flatMap(digest => utxoT.map(utxo => new WrappedDigestState(digest, utxo, settings)))
}
