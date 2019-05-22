package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.PoPowAlgos._
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header}
import org.ergoplatform.settings.ErgoValidationSettings
import org.ergoplatform.settings.ValidationRules._
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, ValidationState}
import scorex.util.bytesToId

class ExtensionValidator(settings: ErgoValidationSettings) extends ScorexEncoding {

  private def validationState: ValidationState[Unit] = ModifierValidator(settings)

  def validateExtension(extension: Extension,
                        header: Header,
                        prevExtensionOpt: Option[ExtensionCandidate],
                        prevHeaderOpt: Option[Header]): ValidationState[Unit] = {
    validateInterlinks(extension, header, prevExtensionOpt, prevHeaderOpt)
      .validate(exKeyLength, extension.fields.forall(_._1.lengthCompare(Extension.FieldKeySize) == 0), extension.encodedId)
      .validate(exValueLength, extension.fields.forall(_._2.lengthCompare(Extension.FieldValueMaxSize) <= 0), extension.encodedId)
      .validate(exDuplicateKeys, extension.fields.map(kv => bytesToId(kv._1)).distinct.length == extension.fields.length, extension.encodedId)
      .validate(exEmpty, header.isGenesis || extension.fields.nonEmpty, extension.encodedId)
  }


  private def validateInterlinks(extension: Extension,
                                 header: Header,
                                 prevExtensionOpt: Option[ExtensionCandidate],
                                 prevHeaderOpt: Option[Header]): ValidationState[Unit] = {
    (prevHeaderOpt, prevExtensionOpt) match {
      case (Some(parent), Some(parentExt)) =>
        val parentLinksTry = unpackInterlinks(parentExt.fields)
        val currentLinksTry = unpackInterlinks(extension.fields)
        validationState
          .validateNoFailure(exIlEncoding, currentLinksTry)
          .validate(exIlStructure, parentLinksTry.flatMap(parLinks => currentLinksTry.map(parLinks -> _))
            .toOption.exists { case (prev, cur) => updateInterlinks(parent, prev) == cur })
      case _ =>
        validationState
          .validate(exIlUnableToValidate, header.isGenesis || prevExtensionOpt.isEmpty)
    }
  }
}
