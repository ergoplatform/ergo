package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.local.{BetterChain, NipopowProofVerificationResult, NipopowVerifier}
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, NipopowProof, NipopowProofSerializer, PoPowHeader, PoPowParams}
import org.ergoplatform.nodeView.history.ErgoHistory.GenesisHeight
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.ChainSettings
import org.ergoplatform.settings.Constants.HashLength
import scorex.core.consensus.ProgressInfo
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, ScorexLogging}

import scala.util.Try

/**
  * Functional component of history storage and processing, which is focused on processing NiPoPoWs and storing
  * corresponding data
  */
trait PopowProcessor extends BasicReaders with ScorexLogging {

  /**
    * @return interface to read objects from history database
    */
  def historyReader: ErgoHistoryReader

  /**
    * @return settings corresponding to ergo { chain { ... }} section of the config
    */
  protected def chainSettings: ChainSettings

  private lazy val nipopowAlgos: NipopowAlgos = new NipopowAlgos(chainSettings)

  /**
    * Binary serializer for NiPoPoW proofs
    */
  lazy val nipopowSerializer = new NipopowProofSerializer(nipopowAlgos)

  private lazy val nipopowVerifier =
    new NipopowVerifier(chainSettings.genesisId.orElse(bestHeaderIdAtHeight(GenesisHeight)))

  protected val NipopowSnapshotHeightKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(50: Byte))

  /**
    * Minimal superchain length ('m' in KMZ17 paper) value used in NiPoPoW proofs for bootstrapping
    */
  val P2PNipopowProofM = 6

  /**
    * Suffix length ('k' in KMZ17 paper) value used in NiPoPoW proofs for bootstrapping
    */
  val P2PNipopowProofK = 10

  /**
    * Checks and appends new header to history
    *
    * @param h - header to process
    * @param nipopowMode
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(h: Header, nipopowMode: Boolean): Try[ProgressInfo[BlockSection]]

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


  /**
    * Constructs PoPoW proof for given m and k according to KMZ17 (FC20 version).
    * See PoPowAlgos.prove for construction details.
    *
    * @param m           - min superchain length
    * @param k           - suffix length
    * @param headerIdOpt - optional header to start suffix from (so to construct proof for the header).
    *                    Please note that k-1 headers will be provided after the header.
    * @return PoPow proof if success, Failure instance otherwise
    */
  def popowProof(m: Int, k: Int, headerIdOpt: Option[ModifierId]): Try[NipopowProof] = {
    val proofParams = PoPowParams(m, k, continuous = true)
    nipopowAlgos.prove(historyReader, headerIdOpt = headerIdOpt)(proofParams)
  }

  /**
    * Constructs PoPoW proof for default m and k
    */
  def popowProof(): Try[NipopowProof] = {
    popowProof(P2PNipopowProofM, P2PNipopowProofK, None)
  }

  /**
    * Constructs PoPoW proof for given m and k and then serializing it
    */
  def popowProofBytes(m: Int, k: Int, headerIdOpt: Option[ModifierId]): Try[Array[Byte]] = {
    popowProof(m, k, headerIdOpt).map(p => nipopowSerializer.toBytes(p))
  }

  /**
    * Constructs PoPoW proof for default m and k and then serializing it
    */
  def popowProofBytes(): Try[Array[Byte]] = {
    popowProofBytes(P2PNipopowProofM, P2PNipopowProofK, None)
  }

  /**
    * Extract headers from proof and initialize history with them
    */
  def applyPopowProof(proof: NipopowProof): Unit = {
    nipopowVerifier.process(proof) match {
      case BetterChain =>
        val headersToApply = nipopowVerifier.bestChain.sortBy(_.height) // sorting could be an overkill, but anyway
        headersToApply.foreach { h =>
          if (!historyReader.contains(h.id)) {
            process(h, nipopowMode = true)
          }
        }
        nipopowVerifier.reset()
        log.info(s"Nipopow proof applied, best header now is ${historyReader.bestHeaderOpt}")
      case r: NipopowProofVerificationResult =>
        log.warn(s"NiPoPoW proof is no better or invalid ($r): $proof")
    }
  }

}
