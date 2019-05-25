package org.ergoplatform.settings

import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.history.{ExtensionCandidate, Extension}
import scorex.core.serialization.{ScorexSerializer, BytesSerializable}
import scorex.core.validation.{ValidationSettings, ModifierValidator, ValidationResult}
import scorex.util.serialization.{Reader, Writer}
import org.ergoplatform.{DisabledRule, ChangedRule, ReplacedRule}
import org.ergoplatform.{RuleStatus => SRuleStatus, ValidationRules => SValidationRules, ValidationSettings => SValidationSettings}
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter, Helpers}

import scala.util.Try

/**
  * Ergo configuration of validation.
  *
  * Specifies the strategy to by used (fail-fast) and
  * validation rules with their statuses
  *
  * @param rules - map from rule id to it's current status
  */
case class ErgoValidationSettings(
    rules: Map[Short, RuleStatus],
    disabledRules: Seq[Short],
    sigmaRules: Map[Short, SRuleStatus]) extends ValidationSettings with BytesSerializable {

  override type M = ErgoValidationSettings

  override val isFailFast: Boolean = true

  override def getError(id: Short, details: String): ValidationResult.Invalid = {
    rules.get(id).map(_.error(details)).getOrElse(ModifierValidator.fatal("Unknown message"))
  }

  override def isActive(id: Short): Boolean = {
    rules.get(id).forall(_.isActive)
  }

  /**
    * Disable sequience of rules
    */
  def disable(ids: Seq[Short]): ErgoValidationSettings = if (ids.nonEmpty) {
    val newRules = rules.map { case rule @ (ruleId, status) =>
      if (ids.contains(ruleId)) {
        ruleId -> status.copy(isActive = false)
      } else {
        rule
      }
    }
    val newSigmaRules = sigmaRules.map { case rule @ (ruleId, status) =>
      if (ids.contains(ruleId)) {
        ruleId -> DisabledRule
      } else {
        rule
      }
    }
    ErgoValidationSettings(newRules, disabledRules, newSigmaRules)
  } else {
    this
  }

  /**
    * Generates extension candidate with serialized ErgoValidationSettings in it
    */
  def toExtensionCandidate(): ExtensionCandidate = {
    val fields = bytes.sliding(Extension.FieldValueMaxSize, Extension.FieldValueMaxSize).zipWithIndex.map { case (b, i) =>
      Array(Extension.ValidationRulesPrefix, i.toByte) -> b
    }
    ExtensionCandidate(fields.toSeq)
  }

  override def serializer: ScorexSerializer[ErgoValidationSettings] = ErgoValidationSettingsSerializer

  override def equals(obj: Any): Boolean = obj match {
    case p: ErgoValidationSettings => ErgoValidationSettings.matchSettings(this, p).isSuccess
    case _ => false
  }
}

object ErgoValidationSettings {
  // TODO optimize: throwing exception by default incur building of a stack trace, which is 10-100x
  //  slowdown comparing to returning Option

  /**
    * Checks that s1 and s2 are equals
    */
  def matchSettings(s1: ErgoValidationSettings, s2: ErgoValidationSettings): Try[Unit] = Try {
    if (s1.rules.size != s2.rules.size) {
      throw new Exception(s"Rules differ in size, s1 = $s1, s2 = $s2")
    }
    s1.rules.foreach { case (k, v) =>
      val v2 = s2.rules(k)
      if (v2 != v) throw new Exception(s"Calculated and received settings differ in $k ($v != $v2)")
    }
  }

  /**
    * Initial validation settings.
    * To be used during genesis state creation or to perform checks that are not allowed
    * to be deactivated via soft-forks.
    */
  val initial: ErgoValidationSettings = new ErgoValidationSettings(ValidationRules.rulesSpec, Seq.empty, Map.empty)

  def parseBytesByPrefix(extension: ExtensionCandidate, prefix: Byte): Array[Byte] = {
    val values = extension.fields
        .filter(_._1(0) == prefix)
        .sortBy(_._1(1))
        .map(_._2)
    Helpers.concatArrays(values)
  }

  /**
    * Extracts ErgoValidationSettings from extension section of the block
    */
  def parseExtension(extension: ExtensionCandidate): Try[ErgoValidationSettings] = Try {
    val ergoRulesBytes = parseBytesByPrefix(extension, Extension.ValidationRulesPrefix)
    val ergoVSettings = ErgoValidationSettingsSerializer.parseBytes(ergoRulesBytes)
    val sigmaRulesBytes = parseBytesByPrefix(extension, Extension.SigmaValidationRulesPrefix)
    if (sigmaRulesBytes.nonEmpty) {
      val r = SigmaSerializer.startReader(sigmaRulesBytes)
      val nRules = r.getUShort()
      val sigmaRules = (0 until nRules).map { _ =>
        val ruleId = r.getShort()
        val status = RuleStatusSerializer.parse(r)
        ruleId -> status
      }
    }

    ergoVSettings
  }

  object RuleStatusSerializer extends SigmaSerializer[SRuleStatus, SRuleStatus] {
    val ReplacedRuleCode = 1
    val ChangedRuleCode = 2

    override def serialize(obj: SRuleStatus, w: SigmaByteWriter): Unit = {
    }

    override def parse(r: SigmaByteReader): SRuleStatus = {
      val statusType = r.getByte()
      statusType match {
        case ReplacedRuleCode =>
          ReplacedRule(r.getShort())
        case ChangedRuleCode =>
          ChangedRule(r.getBytes(r.remaining))
      }
    }
  }

  def toSigmaValidationSettings(ergoVS: ErgoValidationSettings): SValidationSettings = {
    val initVs = SValidationRules.currentSettings
    val sigmaRules = ergoVS.rules.filter { case (id, _) => initVs.get(id).isDefined }
    val res = sigmaRules.foldLeft(initVs) { case (vs, (id, status)) =>
      val vs1 = if (status.isActive) {
        status.ruleDataOpt match {
          case Some(data) =>
            val sigmaStatus = RuleStatusSerializer.parse(SigmaSerializer.startReader(data))
            vs.updated(id, sigmaStatus)
          case None =>
            vs
        }
      } else {
        vs.updated(id, DisabledRule)
      }
      vs1
    }
    res
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
