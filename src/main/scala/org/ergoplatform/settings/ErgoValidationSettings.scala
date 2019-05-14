package org.ergoplatform.settings

import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.history.Extension
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.serialization.ScorexSerializer
import scorex.core.validation.{ModifierValidator, ValidationResult, ValidationSettings}
import scorex.util.serialization.{Reader, Writer}

import scala.util.Try

class ErgoValidationSettings(map: Map[Short, RuleStatus]) extends ValidationSettings {


  override val isFailFast: Boolean = true

  override def getError(id: Short, details: String): ValidationResult.Invalid = {
    map.get(id).map(_.error(details)).getOrElse(ModifierValidator.fatal("Unknown message"))
  }

  override def isActive(id: Short): Boolean = {
    map.get(id).forall(_.isActive)
  }

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
