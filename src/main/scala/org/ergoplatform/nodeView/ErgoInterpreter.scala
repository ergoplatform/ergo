package org.ergoplatform.nodeView

import org.ergoplatform.ErgoLikeContext.{Height, Metadata}
import org.ergoplatform._
import org.ergoplatform.settings.{Constants, Parameters}
import sigmastate.{AvlTreeData, SBoolean}
import sigmastate.Values.Value
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.VerificationResult

import scala.util.Try

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

  //Check that expired box is spent in a proper way
  protected def checkExpiredBox(box: ErgoBox, output: ErgoBoxCandidate): Boolean = {
    val maxStorageFee = Parameters.K * box.bytes.length * (output.creationHeight - box.creationHeight)

    (box.value - maxStorageFee <= 0 && box.value == output.value) || {
      output.value >= box.value - maxStorageFee &&
        ErgoBox.allRegisters.tail.forall(rId => rId == ErgoBox.ReferenceRegId || box.get(rId) == output.get(rId))
    }
  }

  override def verify(exp: Value[SBoolean.type],
                      context: CTX,
                      proof: Array[Byte],
                      message: Array[Byte]): Try[VerificationResult] = {

    lazy val varId = Constants.StorageIndexVarId

    //no proof provided and enough time since box cretion to spend it
    if (context.currentHeight - context.self.creationHeight >= Constants.StoragePeriod
        && proof.length == 0
        && context.extension.values.contains(varId)) {

      Try {
        val idx = context.extension.values(varId).value.asInstanceOf[Short]
        val outputCandidate = context.spendingTransaction.outputCandidates(idx)

        checkExpiredBox(context.self, outputCandidate) -> Constants.StorageContractCost
      }.recoverWith{case _ => super.verify(exp, context, proof, message)}
    } else {
      super.verify(exp, context, proof, message)
    }
  }
}

object ErgoInterpreter {
  val instance = new ErgoInterpreter()
}
