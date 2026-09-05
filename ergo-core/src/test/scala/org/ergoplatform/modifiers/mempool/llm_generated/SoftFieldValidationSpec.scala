package org.ergoplatform.modifiers.mempool

import java.lang.reflect.InvocationTargetException
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeContext, Input}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants._
import org.ergoplatform.validation.SoftFieldsAccessError
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.exceptions.SoftFieldAccessException
import sigma.interpreter.ProverResult
import sigmastate.interpreter.Interpreter.{ScriptEnv, VerificationResult}
import scala.util.{Failure, Try}

class SoftFieldValidationSpec extends ErgoCorePropertyTest {
  private val box = new ErgoBox(
    value = 1000000000L, ergoTree = TrueTree, creationHeight = 0,
    additionalTokens = Colls.emptyColl, additionalRegisters = Map.empty,
    transactionId = bytesToId(Array.fill(32)(1.toByte)), index = 0
  )
  private val tx = ErgoTransaction(
    IndexedSeq(Input(box.id, ProverResult.empty)),
    IndexedSeq(new ErgoBoxCandidate(box.value, TrueTree, emptyStateContext.currentHeight))
  )

  private def failureReturnedBy(cause: Throwable): Throwable = {
    implicit val verifier: ErgoInterpreter = new ErgoInterpreter(parameters) {
      override def verify(env: ScriptEnv, exp: ErgoTree, context: ErgoLikeContext,
                          proof: Array[Byte], message: Array[Byte]): Try[VerificationResult] = Failure(cause)
    }
    val result = tx.statefulValidity(IndexedSeq(box), IndexedSeq.empty, emptyStateContext,
      accumulatedCost = 0L, softFieldsAllowed = false)
    result.isFailure shouldBe true
    result.failed.get
  }

  property("direct soft-field failures retain their classification and cause") {
    val cause = new SoftFieldAccessException("minerPubKey")
    val error = failureReturnedBy(cause)
    error shouldBe a[SoftFieldsAccessError]
    error.getCause shouldBe cause
  }

  property("reflection-wrapped soft-field failures retain their classification and cause") {
    val cause = new SoftFieldAccessException("minerPubKey")
    val error = failureReturnedBy(new InvocationTargetException(cause))
    error shouldBe a[SoftFieldsAccessError]
    error.getCause shouldBe cause
  }

  property("nested reflection calls preserve the soft-field classification") {
    val cause = new SoftFieldAccessException("timestamp")
    val error = failureReturnedBy(new InvocationTargetException(new InvocationTargetException(cause)))
    error shouldBe a[SoftFieldsAccessError]
    error.getCause shouldBe cause
  }

  property("unrelated interpreter failures retain ordinary rejection") {
    Seq(new IllegalArgumentException("invalid input"),
      new InvocationTargetException(new IllegalArgumentException("invalid input")),
      new InvocationTargetException(null),
      new IllegalStateException(new SoftFieldAccessException("minerPubKey"))).foreach { cause =>
      failureReturnedBy(cause).isInstanceOf[SoftFieldsAccessError] shouldBe false
    }
  }

  property("reflection unwrapping observes its depth limit") {
    def wrapped(depth: Int): Throwable = (1 to depth).foldLeft[Throwable](
      new SoftFieldAccessException("minerPubKey")) { (cause, _) => new InvocationTargetException(cause) }
    failureReturnedBy(wrapped(16)) shouldBe a[SoftFieldsAccessError]
    failureReturnedBy(wrapped(17)).isInstanceOf[SoftFieldsAccessError] shouldBe false
  }
}
