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
  * @param reemissionTokenId - id of the EIP-27 re-emission token, when re-emission rules are
  *                          active on this chain (None otherwise). Used by the storage-rent
  *                          repairs (block version `Constants.StorageRentRepairsBlockVersion`+)
  *                          to drop the token from recreated expired boxes and release its
  *                          nanoErg equivalent from the recreation floor.
  */
class ErgoInterpreter(params: BlockchainParameters,
                      val reemissionTokenId: Option[Coll[Byte]] = None)
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
    * Checks that expired box is spent in a proper way
    *
    * @param box           - box being spent
    * @param output        - newly created box when storage fee covered, otherwise any output box
    * @param currentHeight - current height of the blockchain (at the moment of spending)
    * @return whether the box is spent properly according to the storage fee rule
    */
  protected def checkExpiredBox(box: ErgoBox, output: ErgoBoxCandidate, currentHeight: Height): Boolean = {
    val repairsActivated = params.blockVersion >= Constants.StorageRentRepairsBlockVersion

    // From `StorageRentRepairsBlockVersion` the storage fee is computed in 64-bit
    // arithmetic. Before activation the product wraps around Int as it always did:
    // the wrap is consensus-observable (it changes which claims are valid), so
    // blocks below the activation version must keep the legacy arithmetic.
    val storageFee: Long =
      if (repairsActivated) {
        params.storageFeeFactor.toLong * box.bytes.length
      } else {
        (params.storageFeeFactor * box.bytes.length).toLong
      }

    // From `StorageRentRepairsBlockVersion`, EIP-27 re-emission tokens carried by
    // an expired box must be dropped from the recreated box (they may not be
    // preserved: `verifyReemissionSpending` forbids any output from carrying
    // them, which previously made such boxes unclaimable via storage rent).
    // Their nanoErg equivalent (1 per token) is released from the recreation
    // floor so the transaction can pay the burn obligation to the
    // pay-to-reemission contract, which `verifyReemissionSpending` enforces
    // transaction-wide.
    val reemissionEntry: Option[(Coll[Byte], Long)] = if (repairsActivated) {
      reemissionTokenId.flatMap { tokenId =>
        val debt = box.additionalTokens.toArray.iterator
          .collect { case (id, amount) if id == tokenId => amount }
          .foldLeft(0L)(Math.addExact)
        if (debt > 0) Some(tokenId -> debt) else None
      }
    } else {
      None
    }
    val reemissionDebt = reemissionEntry.map(_._2).getOrElse(0L)

    // Compare before the second subtraction so a maximal token amount cannot
    // underflow Long and turn a fully consumable box into the recreation branch.
    val valueAfterStorageFee = box.value - storageFee
    val storageFeeNotCovered = valueAfterStorageFee <= reemissionDebt
    lazy val correctCreationHeight = output.creationHeight == currentHeight
    lazy val correctOutValue = output.value >= valueAfterStorageFee - reemissionDebt

    // all the registers except of R0 (monetary value) and R3 (creation height and reference) must be
    // preserved; once the storage-rent repairs are activated, R2 (tokens) must instead equal the
    // input's tokens with the re-emission token dropped (when the box carries it)
    lazy val correctRegisters = ErgoBox.allRegisters
      .iterator
      .forall { rId =>
        rId == ErgoBox.ValueRegId || rId == ErgoBox.ReferenceRegId || {
          reemissionEntry match {
            case Some((tokenId, _)) if rId == ErgoBox.TokensRegId =>
              val expectedTokens =
                box.additionalTokens.toArray.filterNot { case (id, _) => id == tokenId }
              val outputTokens = output.additionalTokens.toArray
              outputTokens.length == expectedTokens.length &&
                outputTokens.indices.forall { i =>
                  outputTokens(i)._1 == expectedTokens(i)._1 && outputTokens(i)._2 == expectedTokens(i)._2
                }
            case _ =>
              box.get(rId) == output.get(rId)
          }
        }
      }

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
  /** Initial cost of instantiating an interpreter and creating ErgoLikeContext.
    * Added once per transaction.
    */
  val interpreterInitCost = 10000

  /** Creates an interpreter with the given parameters. */
  def apply(params: BlockchainParameters): ErgoInterpreter =
    new ErgoInterpreter(params)

  /**
    * Creates an interpreter with the given parameters and the chain's EIP-27
    * re-emission token id (when re-emission rules are active), enabling the
    * storage-rent repairs semantics from
    * `Constants.StorageRentRepairsBlockVersion`.
    */
  def apply(params: BlockchainParameters, reemissionTokenId: Option[Coll[Byte]]): ErgoInterpreter =
    new ErgoInterpreter(params, reemissionTokenId)

  /** Create [[AvlTreeData]] with the given digest and all operations enabled. */
  def avlTreeFromDigest(digest: Coll[Byte]): AvlTreeData = {
    val flags = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)
    AvlTreeData(digest, flags, Constants.HashLength)
  }

}
