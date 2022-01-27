package org.ergoplatform.nodeView.state.wrapped

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import scorex.core.VersionTag

import scala.util.Try

class WrappedDigestState(val digestState: DigestState,
                         val wrappedUtxoState: WrappedUtxoState,
                         val settings: ErgoSettings,
                         override val parameters: Parameters)
  extends DigestState(digestState.version, digestState.rootHash, digestState.store, parameters, settings) {

  override def applyModifier(mod: ErgoPersistentModifier)
                            (generate: LocallyGeneratedModifier => Unit): Try[WrappedDigestState] = {
    wrapped(super.applyModifier(mod)(_ => ()), wrappedUtxoState.applyModifier(mod)(_ => ()))
  }

  override def rollbackTo(version: VersionTag): Try[WrappedDigestState] = {
    wrapped(super.rollbackTo(version), wrappedUtxoState.rollbackTo(version))
  }

  private def wrapped(digestT: Try[DigestState], utxoT: Try[WrappedUtxoState]): Try[WrappedDigestState] =
    digestT.flatMap(digest => utxoT.map(utxo => new WrappedDigestState(digest, utxo, settings, parameters)))
}
