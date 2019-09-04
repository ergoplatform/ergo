package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.history.{PoPowAlgos, PoPowProof}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import org.ergoplatform.settings.PoPowParams
import scorex.util.ScorexLogging

import scala.util.Try

trait ProvingPoPowComponent extends EmptyPoPowComponent {
  self: HeadersProcessor with ScorexLogging =>

  override def prove(params: PoPowParams): Try[PoPowProof] =
    Try(PoPowAlgos.prove(Seq.empty)(params))

}
