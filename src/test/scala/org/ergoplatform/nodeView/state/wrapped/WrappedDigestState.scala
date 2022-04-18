package org.ergoplatform.nodeView.state.wrapped

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.settings.ErgoSettings
import scorex.core.VersionTag

import scala.util.Try

class WrappedDigestState(val digestState: DigestState,
                         val wrappedUtxoState: WrappedUtxoState,
                         val settings: ErgoSettings)
  extends DigestState(digestState.version, digestState.rootHash, digestState.store, settings) {

  override def applyModifier(mod: ErgoPersistentModifier, estimatedTip: Option[Height])
                            (generate: LocallyGeneratedModifier => Unit): Try[WrappedDigestState] = {
    wrapped(super.applyModifier(mod, estimatedTip)(_ => ()), wrappedUtxoState.applyModifier(mod, estimatedTip)(_ => ()))
  }

  override def rollbackTo(version: VersionTag): Try[WrappedDigestState] = {
    wrapped(super.rollbackTo(version), wrappedUtxoState.rollbackTo(version))
  }

  private def wrapped(digestT: Try[DigestState], utxoT: Try[WrappedUtxoState]): Try[WrappedDigestState] =
    digestT.flatMap(digest => utxoT.map(utxo => new WrappedDigestState(digest, utxo, settings)))
}
