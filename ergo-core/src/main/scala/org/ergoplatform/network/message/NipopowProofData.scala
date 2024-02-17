package org.ergoplatform.network.message

import org.ergoplatform.settings.Algos
import scorex.util.ModifierId

case class NipopowProofData(m: Int, k: Int, headerId: Option[ModifierId]) {
  def headerIdBytesOpt: Option[Array[Byte]] = headerId.map(Algos.decode).flatMap(_.toOption)
}
