package org.ergoplatform.settings

import org.ergoplatform.api.ApiCodecs
import scorex.core.serialization.ScorexSerializer
import scorex.core.validation.{ModifierValidator, ValidationResult, ValidationSettings}
import scorex.util.serialization.{Reader, Writer}


case class ErgoValidationSettings(rules: Map[Short, RuleStatus]) extends ValidationSettings {

  override val isFailFast: Boolean = true

  override def getError(id: Short, details: String): ValidationResult.Invalid = {
    rules.get(id).map(_.error(details)).getOrElse(ModifierValidator.fatal("Unknown message"))
  }

  override def isActive(id: Short): Boolean = {
    rules.get(id).forall(_.isActive)
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
