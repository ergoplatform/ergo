package org.ergoplatform.wallet.interpreter

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.sdk.BlockchainParameters
import org.ergoplatform.wallet.protocol.Constants
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeContext, ErgoLikeInterpreter}
import scorex.util.ScorexLogging
import sigmastate.interpreter.Interpreter.{ScriptEnv, VerificationResult}
import sigma.Coll
import sigma.ast.ErgoTree
import sigma.data.{AvlTreeData, AvlTreeFlags}

import scala.util.Try

/**
  * ErgoTree language interpreter, Ergo version. In addition to ErgoLikeInterpreter, it contains
  * rules for expired boxes spending validation.
  *
  * @param params - current values of adjustable blockchain settings
  */
class ErgoInterpreter(params: BlockchainParameters)
  extends ErgoLikeInterpreter with ScorexLogging {

  /** Override default logging for all Ergo interpreters. */
  override protected def logMessage(msg: String): Unit = {
    log.error(msg)
  }
  override protected def logMessage(msg: String, t: Throwable): Unit = {
    log.error(msg, t)
  }

  override type CTX = ErgoLikeContext

  /**
    * After an expired box holding tokens or additional registers is charged down to the minimum
    * allowed value, for how many blocks it can still be refreshed with preserving tokens / registers.
    */
  val StorageGracePeriod: Int = 2 * Constants.BlocksPerWeek

  /**
    * Height starting from which the grace-period rules apply to expired boxes holding tokens or
    * additional registers
    *
    * TODO: set before releasesponsored (recreated with a topped-up
    * value) before it may be fully consumed. 2 weeks
    */
  val StorageGracePeriodActivationHeight: Int = 2100000

  /**
    * Checks that expired box is spent in a proper way
    *
    * @param box           - box being spent
    * @param output        - newly created box when storage fee covered, otherwise any output box
    * @param currentHeight - current height of the blockchain (at the moment of spending)
    * @return whether the box is spent properly according to the storage fee rule
    */
  protected def checkExpiredBox(box: ErgoBox, output: ErgoBoxCandidate, currentHeight: Height): Boolean = {
    // The grace-period rules protect only boxes holding tokens or additional registers.    // : for them the original owner's data and assets may matter to others.    // : for them the original owner's data and assets may matter to others.
    // Plain value boxes are charged and consumed under the original rules.
    val graceRulesApply = currentHeight >= StorageGracePeriodActivationHeight &&
      (box.additionalTokens.nonEmpty || box.additionalRegisters.nonEmpty)
    if (graceRulesApply) {
      checkExpiredBoxWithGracePeriod(box, output, currentHeight)
    } else {
      checkExpiredBoxOriginal(box, output, currentHeight)
    }
  }

  /**
    * Original expired-box spending rules: the storage fee is charged in full on recreation, and
    * if the box value does not cover the fee, the box may be fully consumed by anyone.
    */
  private def checkExpiredBoxOriginal(box: ErgoBox,
                                      output: ErgoBoxCandidate,
                                      currentHeight: Height): Boolean = {
    val storageFee = params.storageFeeFactor * box.bytes.length

    val storageFeeNotCovered = box.value - storageFee <= 0
    lazy val correctCreationHeight = output.creationHeight == currentHeight
    lazy val correctOutValue = output.value >= box.value - storageFee

    // all the registers except of R0 (monetary value) and R3 (creation height and reference) must be preserved
    lazy val correctRegisters = ErgoBox.allRegisters
      .iterator
      .forall(rId => rId == ErgoBox.ValueRegId || rId == ErgoBox.ReferenceRegId || box.get(rId) == output.get(rId))

    storageFeeNotCovered || (correctCreationHeight && correctOutValue && correctRegisters)
  }

  /**
    * Storage rent rules with the grace period: an expired box may be charged only down to the
    * minimum allowed value (`minValuePerByte` per box byte). If the box value does not cover
    * the storage fee, the box may be fully consumed (burned) only after a grace period
    * (`StorageGracePeriod` after the storage period is over); until then it may be recreated,
    * keeping at least the minimum value, topped up by a sponsor if needed. Every spend valid
    * under these rules is also valid under the original rules.
    */
  private def checkExpiredBoxWithGracePeriod(box: ErgoBox,
                                             output: ErgoBoxCandidate,
                                             currentHeight: Height): Boolean = {
    val storageFee = params.storageFeeFactor * box.bytes.length
    val minValue = params.minValuePerByte.toLong * box.bytes.length

    val gracePeriodPassed =
      currentHeight - box.creationHeight >= Constants.StoragePeriod + StorageGracePeriod
    val storageFeeNotCovered = box.value - storageFee <= 0
    val burnable = storageFeeNotCovered && gracePeriodPassed

    lazy val correctCreationHeight = output.creationHeight == currentHeight
    lazy val correctOutValue = output.value >= Math.max(box.value - storageFee, minValue)

    // all the registers except of R0 (monetary value) and R3 (creation height and reference) must be preserved
    lazy val correctRegisters = ErgoBox.allRegisters
      .iterator
      .forall(rId => rId == ErgoBox.ValueRegId || rId == ErgoBox.ReferenceRegId || box.get(rId) == output.get(rId))

    burnable || (correctCreationHeight && correctOutValue && correctRegisters)
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
  /** Initial cost of instantiating an interpreter and creating ErgoLikeContext.
    * Added once per transaction.
    */
  val interpreterInitCost = 10000

  /** Creates an interpreter with the given parameters. */
  def apply(params: BlockchainParameters): ErgoInterpreter =
    new ErgoInterpreter(params)

  /** Create [[AvlTreeData]] with the given digest and all operations enabled. */
  def avlTreeFromDigest(digest: Coll[Byte]): AvlTreeData = {
    val flags = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)
    AvlTreeData(digest, flags, Constants.HashLength)
  }

}
