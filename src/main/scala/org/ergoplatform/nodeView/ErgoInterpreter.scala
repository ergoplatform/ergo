package org.ergoplatform.nodeView

import org.ergoplatform.ErgoLikeContext.{Height, Metadata}
import org.ergoplatform._
import org.ergoplatform.settings.{Constants, Parameters}
import sigmastate.{AvlTreeData, SBoolean}
import sigmastate.Values.Value
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.VerificationResult

import scala.util.{Success, Try}

class ErgoContext(override val currentHeight: Height,
                  override val lastBlockUtxoRoot: AvlTreeData,
                  override val boxesToSpend: IndexedSeq[ErgoBox],
                  override val spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                  override val self: ErgoBox,
                  override val metadata: Metadata,
                  override val extension: ContextExtension = ContextExtension(Map()))
  extends ErgoLikeContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, spendingTransaction, self, metadata, extension) {

  override def withExtension(newExtension: ContextExtension): ErgoContext =
    new ErgoContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, spendingTransaction, self, metadata, newExtension)

  override def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoContext =
    new ErgoContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, newSpendingTransaction, self, metadata, extension)
}

class ErgoInterpreter(override val maxCost: Long = Parameters.MaxBlockCost)
  extends ErgoLikeInterpreter(maxCost) {

  override type CTX = ErgoContext

  override def verify(exp: Value[SBoolean.type],
                      context: CTX,
                      proof: Array[Byte],
                      message: Array[Byte]): Try[VerificationResult] = {
    //no proof provided and enough time since box cretion to spend it
    if (context.currentHeight - context.self.creationHeight >= Constants.StoragePeriod && proof.length == 0) {
      Success(true -> 0L) //todo: zero cost to validate the spending condition - is it okay?
    } else {
      super.verify(exp, context, proof, message)
    }
  }
}

object ErgoInterpreter {
  val instance = new ErgoInterpreter()
}
