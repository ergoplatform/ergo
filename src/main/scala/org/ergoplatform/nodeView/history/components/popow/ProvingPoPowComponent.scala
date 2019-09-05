package org.ergoplatform.nodeView.history.components.popow

import org.ergoplatform.modifiers.history.{PoPowAlgos, PoPowProof}
import org.ergoplatform.nodeView.history.components.HeadersComponent
import org.ergoplatform.settings.PoPowParams
import scorex.util.ScorexLogging

import scala.util.Try

trait ProvingPoPowComponent extends EmptyPoPowComponent {
  self: HeadersComponent with ScorexLogging =>

  // todo: create proof and save to db
  override def prove(params: PoPowParams): Try[PoPowProof] =
    Try(PoPowAlgos.prove(Seq.empty)(params))

}
