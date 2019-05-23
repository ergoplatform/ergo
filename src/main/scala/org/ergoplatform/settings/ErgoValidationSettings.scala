package org.ergoplatform.settings

import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.serialization.ScorexSerializer
import scorex.core.validation.{ModifierValidator, ValidationResult, ValidationSettings}
import scorex.util.serialization.{Reader, Writer}

import scala.util.Try


case class ErgoValidationSettings(rules: Map[Short, RuleStatus]) extends ValidationSettings {

  override val isFailFast: Boolean = true

  override def getError(id: Short, details: String): ValidationResult.Invalid = {
    rules.get(id).map(_.error(details)).getOrElse(ModifierValidator.fatal("Unknown message"))
  }

  override def isActive(id: Short): Boolean = {
    rules.get(id).forall(_.isActive)
  }

  def disable(ids: Seq[Short]): ErgoValidationSettings = if (ids.nonEmpty) {
    val newRules = rules.map { currentRule =>
      if (ids.contains(currentRule._1)) {
        currentRule._1 -> currentRule._2.copy(isActive = false)
      } else {
        currentRule
      }
    }
    ErgoValidationSettings(newRules)
  } else {
    this
  }

  def toExtensionCandidate(): ExtensionCandidate = {
    ???
  }

}

object ErgoValidationSettings {

  /**
    * Initial validation settings.
    * To be used during genesis state creation or to perform checks that are not allowed
    * to be deactivated via soft-forks.
    */
  val initial: ErgoValidationSettings = new ErgoValidationSettings(ValidationRules.rulesSpec)

  def parseExtension(h: Height, extension: ExtensionCandidate): Try[ErgoValidationSettings] = Try {
    ???
  }
}

object ErgoValidationSettingsSerializer extends ScorexSerializer[ErgoValidationSettings] with ApiCodecs {
  override def serialize(obj: ErgoValidationSettings, w: Writer): Unit = {
    val disabledRules = obj.rules.filter(r => !r._2.isActive)
    w.putInt(disabledRules.size)
    disabledRules.foreach { r =>
      w.putShort(r._1)
    }
  }

  override def parse(r: Reader): ErgoValidationSettings = {
    val disabledRulesNum = r.getInt()
    val disabledRules = (0 until disabledRulesNum).map { _ =>
      r.getShort()
    }
    ErgoValidationSettings.initial.disable(disabledRules)
  }

}
