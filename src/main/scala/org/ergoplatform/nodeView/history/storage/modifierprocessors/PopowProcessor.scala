package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.local.NipopowVerifier
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, PoPowHeader}
import org.ergoplatform.settings.ChainSettings
import scorex.util.ModifierId


trait PopowProcessor extends BasicReaders {

  val P2PNipopowProofM = 6
  val P2PNipopowProofK = 6

  protected def chainSettings: ChainSettings

  val powScheme: AutolykosPowScheme

  val nipopowAlgos: NipopowAlgos = new NipopowAlgos(powScheme)

  lazy val nipopowVerifier = new NipopowVerifier(chainSettings.genesisId.get) // todo: get

  /**
    * Constructs popow header against given header identifier
    *
    * @param headerId - identifier of the header
    * @return PoPowHeader(header + interlinks + interlinkProof) or
    *         None if header of extension of a corresponding block are not available
    */
  def popowHeader(headerId: ModifierId): Option[PoPowHeader] = {
    typedModifierById[Header](headerId).flatMap(h =>
      typedModifierById[Extension](h.extensionId).flatMap { ext =>
        val interlinks = NipopowAlgos.unpackInterlinks(ext.fields).toOption
        val interlinkProof = NipopowAlgos.proofForInterlinkVector(ext)
        (interlinks, interlinkProof) match {
          case (Some(links), Some(proof)) => Some(PoPowHeader(h, links, proof))
          case _ => None
        }
      }
    )
  }

  /**
    * Constructs popow header (header + interlinks) for еру best header at given height
    *
    * @param height - height
    * @return PoPowHeader(header + interlinks) or None if header of extension of a corresponding block are not available
    */
  def popowHeader(height: Int): Option[PoPowHeader] = {
    bestHeaderIdAtHeight(height).flatMap(popowHeader)
  }


}
