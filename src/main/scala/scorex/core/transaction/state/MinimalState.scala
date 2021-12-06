package scorex.core.transaction.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.{PersistentNodeViewModifier, VersionTag}

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait MinimalState[MS <: MinimalState[MS]] extends StateReader {
  self: MS =>

  def applyModifier(mod: ErgoPersistentModifier): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]

  /**
    * @return read-only copy of this state
    */
  def getReader: StateReader = this

}


trait StateFeature

trait TransactionValidation extends StateFeature {
  def validate(tx: ErgoTransaction): Try[Unit]
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}
