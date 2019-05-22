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

  def parseExtension(h: Height, extension: Extension): Try[ErgoValidationSettings] = Try {
    ???
  }
}

object ErgoValidationSettingsSerializer extends ScorexSerializer[ErgoValidationSettings] with ApiCodecs {
  override def serialize(obj: ErgoValidationSettings, w: Writer): Unit = {
    w.putInt(obj.rules.size)
    obj.rules.toSeq.sortBy(_._1).foreach { r =>
      w.putShort(r._1)
      if (r._2.isActive) w.put(1.toByte) else w.put(0.toByte)
    }
  }

  override def parse(r: Reader): ErgoValidationSettings = {
    val ruleNum = r.getInt()
    val rules = (0 until ruleNum).map { _ =>
      val id = r.getShort()
      val isActive = if (r.getByte() == 1.toByte) true else false
      val ruleSpec = ValidationRules.rulesSpec(id)
      id -> ruleSpec.copy(isActive = isActive)
    }
    ErgoValidationSettings(rules.toMap)
  }

}
