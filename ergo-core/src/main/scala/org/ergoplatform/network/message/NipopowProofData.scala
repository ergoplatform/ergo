package org.ergoplatform.network.message

import org.ergoplatform.settings.Algos
import scorex.util.ModifierId

/**
  * Request for a NiPoPoW proof.
  *
  * @param pad - bytes after the parameters, which a node that does not know them reads and skips. A first
  *            byte of `NipopowProofData.CurrentTip` asks for a proof of the chain up to the current best
  *            header, where a request without it is answered with the proof the node stored for the last UTXO
  *            set snapshot.
  */
case class NipopowProofData(m: Int, k: Int, headerId: Option[ModifierId], pad: Array[Byte] = Array.emptyByteArray) {
  def headerIdBytesOpt: Option[Array[Byte]] = headerId.map(Algos.decode).flatMap(_.toOption)

  def wantsCurrentTip: Boolean = pad.headOption.contains(NipopowProofData.CurrentTip)
}

object NipopowProofData {
  val CurrentTip: Byte = 1
}
