package org.ergoplatform.wallet.interpreter

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.wallet.protocol.Constants
import org.ergoplatform.wallet.protocol.context.ErgoLikeParameters
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeContext, ErgoLikeInterpreter}
import scorex.crypto.authds.ADDigest
import sigmastate.Values.ErgoTree
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.Interpreter.{ScriptEnv, VerificationResult}
import sigmastate.{AvlTreeData, AvlTreeFlags}

import scala.util.Try

/**
  * ErgoTree language interpreter, Ergo version. In addition to ErgoLikeInterpreter, it contains
  * rules for expired boxes spending validation.
  *
  * @param params - current values of adjustable blockchain settings
  */
class ErgoInterpreter(params: ErgoLikeParameters)(implicit IR: IRContext)
  extends ErgoLikeInterpreter {

  override type CTX = ErgoLikeContext

  /**
    * Check that expired box is spent in a proper way
    *
    * @param box           - box being spent
    * @param output        - newly created box when storage fee covered, otherwise any output box
    * @param currentHeight - current height of the blockchain (at the moment of spending)
    * @return whether the box is spent properly according to the storage fee rule
    */
  protected def checkExpiredBox(box: ErgoBox, output: ErgoBoxCandidate, currentHeight: Height): Boolean = {
    val storageFee = params.storageFeeFactor * box.bytes.length

    val storageFeeNotCovered = box.value - storageFee <= 0
    val correctCreationHeight = output.creationHeight == currentHeight
    val correctOutValue = output.value >= box.value - storageFee

    // all the registers except of R0 (monetary value) and R3 (creation height and reference) must be preserved
    val correctRegisters = ErgoBox.allRegisters.tail
      .forall(rId => rId == ErgoBox.ReferenceRegId || box.get(rId) == output.get(rId))

    storageFeeNotCovered || (correctCreationHeight && correctOutValue && correctRegisters)
  }

  /**
    * Checks that given exp evaluates to `true`.
    *
    * @param env     - environment to use during expression evaluation
    * @param exp     - expression to check
    * @param context - expression evaluation context
    * @param proof   - cryptographic proof
    * @param message - message
    */
  override def verify(env: ScriptEnv,
                      exp: ErgoTree,
                      context: CTX,
                      proof: Array[Byte],
                      message: Array[Byte]): Try[VerificationResult] = {

    val varId = Constants.StorageIndexVarId
    val hasEnoughTimeToBeSpent = context.preHeader.height - context.self.creationHeight >= Constants.StoragePeriod
    // No spending proof provided and enough time since box creation to spend it
    // In this case anyone can spend the expired box by providing in context extension variable #127 (stored in input)
    //    an index of a recreated box (or index of any box if the value in the expired box isn't enough to pay for the storage fee)
    if (hasEnoughTimeToBeSpent && proof.length == 0 && context.extension.values.contains(varId)) {
      Try {
        val idx = context.extension.values(varId).value.asInstanceOf[Short]
        val outputCandidate = context.spendingTransaction.outputCandidates(idx)
        checkExpiredBox(context.self, outputCandidate, context.preHeader.height) -> Constants.StorageContractCost
      }.recoverWith { case _ =>
        super.verify(env, exp, context, proof, message)
      }
    } else {
      super.verify(env, exp, context, proof, message)
    }
  }

}

object ErgoInterpreter {

  def apply(params: ErgoLikeParameters): ErgoInterpreter =
    new ErgoInterpreter(params)(new RuntimeIRContext)

  def avlTreeFromDigest(digest: ADDigest): AvlTreeData = {
    val flags = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)
    AvlTreeData(digest, flags, Constants.HashLength)
  }

}
