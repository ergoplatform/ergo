package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.local.NipopowVerifier
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, NipopowProof, NipopowProofSerializer, PoPowHeader, PoPowParams}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.ChainSettings
import org.ergoplatform.settings.Constants.HashLength
import scorex.core.consensus.ProgressInfo
import scorex.db.ByteArrayWrapper
import scorex.util.ModifierId

import scala.util.Try


trait PopowProcessor extends BasicReaders {

  protected val historyStorage: HistoryStorage

  val NipopowSnapshotHeightKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(50: Byte))

  val P2PNipopowProofM = 10
  val P2PNipopowProofK = 6

  protected def chainSettings: ChainSettings

  val powScheme: AutolykosPowScheme

  val nipopowAlgos: NipopowAlgos = new NipopowAlgos(powScheme)
  val nipopowSerializer = new NipopowProofSerializer(nipopowAlgos)

  // todo: NipopowVerifier holds nipopow proof in memory without releasing, fix
  lazy val nipopowVerifier = new NipopowVerifier(chainSettings.genesisId.get) // todo: get

  def historyReader: ErgoHistoryReader

  protected def process(h: Header): Try[ProgressInfo[BlockSection]]

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
    * @param m - min superchain length
    * @param k - suffix length
    * @param headerIdOpt - optional header to start suffix from (so to construct proof for the header).
    *                    Please note that k-1 headers will be provided after the header.
    * @return PoPow proof if success, Failure instance otherwise
    */
  def popowProof(m: Int, k: Int, headerIdOpt: Option[ModifierId]): Try[NipopowProof] = {
    val proofParams = PoPowParams(m, k)
    nipopowAlgos.prove(historyReader, headerIdOpt = headerIdOpt)(proofParams)
  }

  def popowProof(): Try[NipopowProof] = {
    popowProof(P2PNipopowProofM, P2PNipopowProofK, None)
  }

  def popowProofBytes(m: Int, k: Int, headerIdOpt: Option[ModifierId]): Try[Array[Byte]] = {
    popowProof(m, k, headerIdOpt).map(p => nipopowSerializer.toBytes(p))
  }

  def popowProofBytes(): Try[Array[Byte]] = {
    popowProofBytes(P2PNipopowProofM, P2PNipopowProofK, None)
  }

  def readPopowFromDb(): Option[NipopowProof] = {
    historyStorage.getIndex(NipopowSnapshotHeightKey).map{bs =>
      nipopowSerializer.parseBytes(bs)
    }
  }

  def applyPopowProof(proof: NipopowProof): Unit = {
    if (nipopowVerifier.process(proof)) {
      val headersToApply = nipopowVerifier.bestChain
      println("to apply: " + headersToApply.length)
      headersToApply.foreach { h =>
        if (!historyStorage.contains(h.id)) {
          println(process(h))
        }
      }
    } else {
      println("!!!") // todo: fix log msg
    }
  }

}
