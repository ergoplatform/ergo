package org.ergoplatform.nodeView

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform._
import org.ergoplatform.settings.{Constants, LaunchParameters, Parameters}
import sigmastate.SBoolean
import sigmastate.Values.Value
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.Interpreter.{ReductionResult, ScriptEnv, VerificationResult}

import scala.util.Try


/**
  * ErgoTree language interpreter, Ergo version. In addition to ErgoLikeInterpreter, it contains
  * rules for expired boxes spending validation.
  *
  * @param params - current values of adjustable blockchain settings
  */
class ErgoInterpreter(params: Parameters)(implicit IR: IRContext)
  extends ErgoLikeInterpreter(params.maxBlockCost) {

  override type CTX = ErgoContext

  /**
    * Check that expired box is spent in a proper way
    *
    * @param box    - box being spent
    * @param output - newly created box
    * @param currentHeight - current height of the blockchain (at the moment of spending)
    * @return whether the box is spent properly according to the storage fee rule
    */
  protected def checkExpiredBox(box: ErgoBox, output: ErgoBoxCandidate, currentHeight: Height): Boolean = {
    val maxStorageFee = params.storageFeeFactor * box.bytes.length

    (box.value - maxStorageFee <= 0) || {
      output.creationHeight == currentHeight &&
        output.value >= box.value - maxStorageFee &&
        ErgoBox.allRegisters.tail.forall(rId => rId == ErgoBox.ReferenceRegId || box.get(rId) == output.get(rId))
    }
  }

  override def verify(env: ScriptEnv,
                      exp: Value[SBoolean.type],
                      context: CTX,
                      proof: Array[Byte],
                      message: Array[Byte]): Try[VerificationResult] = {

    lazy val varId = Constants.StorageIndexVarId

    //no proof provided and enough time since box creation to spend it
    if (context.currentHeight - context.self.creationHeight >= Constants.StoragePeriod
      && proof.length == 0
      && context.extension.values.contains(varId)) {

      Try {
        val idx = context.extension.values(varId).value.asInstanceOf[Short]
        val outputCandidate = context.spendingTransaction.outputCandidates(idx)

        checkExpiredBox(context.self, outputCandidate, context.currentHeight) -> Constants.StorageContractCost
      }.recoverWith { case _ => super.verify(env, exp, context, proof, message) }
    } else {
      super.verify(env, exp, context, proof, message)
    }
  }
}

object ErgoInterpreter {

  def apply(params: Parameters): ErgoInterpreter =
    new ErgoInterpreter(params)(new RuntimeIRContext)
}
