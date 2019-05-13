package org.ergoplatform.settings

import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.history.Extension
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.serialization.ScorexSerializer
import scorex.core.validation.MapValidationSettings
import scorex.core.validation.ValidationResult.Invalid
import scorex.util.serialization.{Reader, Writer}

import scala.util.Try

class ErgoValidationSettings(map: Map[Short, (String => Invalid, Boolean)]) extends MapValidationSettings(true, map) {

  def update(height: Height, forkVote: Boolean, epochVotes: Seq[(Byte, Int)], votingSettings: VotingSettings): ErgoValidationSettings = {
    //TODO
    this
  }

}

object ErgoValidationSettings {

  def parseExtension(h: Height, extension: Extension): Try[ErgoValidationSettings] = Try {
    ???
  }
}

object ErgoValidationSettingsSerializer extends ScorexSerializer[ErgoValidationSettings] with ApiCodecs {
  override def serialize(obj: ErgoValidationSettings, w: Writer): Unit = ???

  override def parse(r: Reader): ErgoValidationSettings = ???
}
