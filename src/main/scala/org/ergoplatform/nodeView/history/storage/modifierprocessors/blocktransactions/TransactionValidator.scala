package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import org.ergoplatform.api.{TransactionInputView, TransactionOutputView, TransactionView}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.validation.{ModifierValidator, ValidationResult}
import scorex.crypto.encode.Base16

object TransactionValidator extends ModifierValidator {

  //todo: testnet1 - fix!
  def validateProto(proto: TransactionView, tx: ErgoTransaction): ValidationResult = ???

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
