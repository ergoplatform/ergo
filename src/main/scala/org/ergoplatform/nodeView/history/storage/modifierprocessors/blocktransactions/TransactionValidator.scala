package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import org.ergoplatform.api.{TransactionInputView, TransactionOutputView, TransactionView}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import scorex.core.validation.{ModifierValidator, ValidationResult}

object TransactionValidator extends ModifierValidator {

  def validateProto(proto: TransactionView, tx: AnyoneCanSpendTransaction): ValidationResult = {
    validateId(proto.id, tx.encodedId, "Transaction Id") ++
      validateIdSeq(proto.inputs.map(_.id), tx.boxIdsToOpen.map(TransactionInputView.idFromADKey), "Input") ++
      validateIdSeq(proto.outputs.map(_.id), tx.newBoxes.map(TransactionOutputView.idFromBox).toSeq, "Output")
  }

  private def validateIdSeq(given: Seq[Option[String]], expected: Seq[String],
                            description: String): ValidationResult = {
    failFast
      .validateEquals(given.length)(expected.length) { detail =>
        fatal(s"$description count doesn't match. $detail")
      }
      .validate {
        (given zip expected)
          .map { case (x, y) => validateId(x, y, s"$description id") }
          .reduceOption(_ ++ _)
          .getOrElse(fatal(s"$description count should not be zero"))
      }
      .result
  }

  private def validateId(givenOpt: Option[String], expected: String, description: String): ValidationResult = {
    givenOpt.fold[ValidationResult](success) { given =>
      accumulateErrors
        .validateEquals(given)(expected) { detail =>
          fatal(s"$description doesn't match. $detail")
        }
        .result
    }
  }

}
