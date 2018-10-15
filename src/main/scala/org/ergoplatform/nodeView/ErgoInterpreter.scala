package org.ergoplatform.nodeView

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform._
import org.ergoplatform.settings.{Constants, Parameters}
import sigmastate.SBoolean
import sigmastate.Values.Value
import sigmastate.interpreter.Interpreter.VerificationResult
import scala.util.Try


class ErgoInterpreter(override val maxCost: Long = Parameters.MaxBlockCost)
  extends ErgoLikeInterpreter(maxCost) {

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
    val maxStorageFee = Parameters.K * box.bytes.length * (output.creationHeight - box.creationHeight)

    (box.value - maxStorageFee <= 0 && box.value == output.value) || {
      output.creationHeight == currentHeight &&
        output.value >= box.value - maxStorageFee &&
        ErgoBox.allRegisters.tail.forall(rId => rId == ErgoBox.ReferenceRegId || box.get(rId) == output.get(rId))
    }
  }

  override def verify(exp: Value[SBoolean.type],
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
      }.recoverWith { case _ => super.verify(exp, context, proof, message) }
    } else {
      super.verify(exp, context, proof, message)
    }
  }
}

object ErgoInterpreter {
  val instance = new ErgoInterpreter()
}
