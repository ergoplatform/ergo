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
    sigmaRules: Map[Short, SRuleStatus]) extends ValidationSettings with BytesSerializable {

  override type M = ErgoValidationSettings

  override val isFailFast: Boolean = true

  /** Sequence of disabled rules ordered by ruleId. */
  def disabledRules: Seq[(Short, RuleStatus)] = rules.filter(r => !r._2.isActive).toSeq.sortBy(_._1)

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
    val newRules = rules.map { currentRule =>
      if (ids.contains(currentRule._1)) {
        currentRule._1 -> currentRule._2.copy(isActive = false)
      } else {
        currentRule
      }
    }
    ErgoValidationSettings(newRules, sigmaRules)
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
  /**
    * Checks that s1 and s2 are equals
    */
  // TODO optimize: throwing exception by default incur building of a stack trace, which is 10-100x
  //  slowdown comparing to returning Option
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
  val initial: ErgoValidationSettings = new ErgoValidationSettings(ValidationRules.rulesSpec, Map.empty)

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
        val ruleId = (r.getUShort() + RuleStatusSerializer.FirstRuleId).toShort
        val status = RuleStatusSerializer.parse(r)
        ruleId -> status
      }
      ergoVSettings.copy(sigmaRules = sigmaRules.toMap)
    } else {
      ergoVSettings
    }
  }

  object RuleStatusSerializer extends SigmaSerializer[SRuleStatus, SRuleStatus] {
    val DisabledRuleCode = 1
    val ReplacedRuleCode = 2
    val ChangedRuleCode = 3
    val FirstRuleId = 1000

    override def serialize(obj: SRuleStatus, w: SigmaByteWriter): Unit = {
    }

    override def parse(r: SigmaByteReader): SRuleStatus = {
      val numBytes = r.getUShort() // read number of bytes occupied by status data
      val statusType = r.getByte()
      statusType match {
        case DisabledRuleCode =>
          DisabledRule  // the rule is explicitly disabled
        case ReplacedRuleCode =>
          val newRule = (r.getUShort() + FirstRuleId).toShort // store small offsets using single byte
          ReplacedRule(newRule) // the rule is disabled, but we also have info about new rule
        case ChangedRuleCode =>
          val bytes = r.getBytes(numBytes - 1) // value bytes except statusType
          ChangedRule(bytes)
        case _ =>
          r.position += numBytes - 1 // skip status bytes which we don't understand
          ReplacedRule(0)  // unrecognized status code, the old code should process it as soft-fork
      }
    }
  }

  def toSigmaValidationSettings(ergoVS: ErgoValidationSettings): SValidationSettings = {
    val initVs = SValidationRules.currentSettings
    val sigmaRules = ergoVS.sigmaRules.filter { rule => initVs.get(rule._1).isDefined }
    val res = sigmaRules.foldLeft(initVs) { (vs, rule) =>
      vs.updated(rule._1, rule._2)
    }
    res
  }

}

/** Serializes mutable state of validation rules as delta from `ErgoValidationSettings.inital` value.
  * To make serialization roundtrip an identity, we require a sequence of disable rules to be sorted.
  * This serializer doesn't save `sigmaRules` and require it to be empty, this is again to enforce
  * a roundtrip identity invariant.
  * @see `disabledRules`
  */
object ErgoValidationSettingsSerializer extends ScorexSerializer[ErgoValidationSettings] with ApiCodecs {
  override def serialize(obj: ErgoValidationSettings, w: Writer): Unit = {
    assert(obj.sigmaRules.isEmpty, s"Non empty sigmaRules: ${obj.sigmaRules}")
    val disabledRules = obj.disabledRules
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
