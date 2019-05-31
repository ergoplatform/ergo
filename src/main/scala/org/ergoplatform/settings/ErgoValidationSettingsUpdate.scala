package org.ergoplatform.settings

import org.ergoplatform.validation.RuleStatusSerializer
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.serialization.ConstantStore
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ErgoValidationSettingsUpdate(rulesToDisable: Seq[Short], statusUpdates: Seq[(Short, org.ergoplatform.validation.RuleStatus)]) {

  def ++(that: ErgoValidationSettingsUpdate): ErgoValidationSettingsUpdate = {
    val newRules = (rulesToDisable ++ that.rulesToDisable).distinct.sorted
    val nonReplacedStatusUpdates = statusUpdates.filter(s => !that.statusUpdates.exists(_._1 == s._1))
    val newStatusUpdates = (nonReplacedStatusUpdates ++ that.statusUpdates).sortBy(_._1)
    ErgoValidationSettingsUpdate(newRules, newStatusUpdates)
  }
}

object ErgoValidationSettingsUpdate {
  val empty: ErgoValidationSettingsUpdate = ErgoValidationSettingsUpdate(Seq(), Seq())
}

object ErgoValidationSettingsUpdateSerializer extends ScorexSerializer[ErgoValidationSettingsUpdate] {

  private val FirstRule = org.ergoplatform.validation.ValidationRules.FirstRuleId

  override def serialize(obj: ErgoValidationSettingsUpdate, w: Writer): Unit = {
    val sigmaWriter = new SigmaByteWriter(w, None)
    w.putUInt(obj.rulesToDisable.length)
    obj.rulesToDisable.foreach { r =>
      w.putUShort(r)
    }

    w.putUInt(obj.statusUpdates.length)
    obj.statusUpdates.foreach { r =>
      w.putUShort(r._1 - FirstRule)
      RuleStatusSerializer.serialize(r._2, sigmaWriter)
    }
  }

  override def parse(r: Reader): ErgoValidationSettingsUpdate = {
    val sigmaReader = new SigmaByteReader(r, new ConstantStore(), resolvePlaceholdersToConstants = false)
    val disabledRulesNum = r.getUInt().toInt
    val disabledRules = (0 until disabledRulesNum).map { _ =>
      r.getUShort().toShort
    }
    val statusUpdatesNum = r.getUInt().toInt
    val parsed = (0 until statusUpdatesNum).map { _ =>
      val ruleId = (r.getUShort() + FirstRule).toShort
      val status = RuleStatusSerializer.parse(sigmaReader)
      ruleId -> status
    }
    ErgoValidationSettingsUpdate(disabledRules, parsed)
  }

}
