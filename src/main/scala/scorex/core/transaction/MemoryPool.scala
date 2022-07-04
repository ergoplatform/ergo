package scorex.core.transaction

import org.ergoplatform.modifiers.mempool.ErgoTransaction

import scala.util.Try

/**
  * Unconfirmed transactions pool
  *
  */
trait MemoryPool[M <: MemoryPool[M]] extends MempoolReader {

  /**
    * Method to put a transaction into the memory pool. Validation of tha transactions against
    * the state is done in NodeVieHolder. This put() method can check whether a transaction is valid
    * @param tx
    * @return Success(updatedPool), if transaction successfully added to the pool, Failure(_) otherwise
    */
  def put(tx: ErgoTransaction): Try[M]

  def put(txs: Iterable[ErgoTransaction]): Try[M]

  def putWithoutCheck(txs: Iterable[ErgoTransaction]): M

  def remove(tx: ErgoTransaction): M

  def filter(txs: Seq[ErgoTransaction]): M = filter(t => !txs.exists(_.id == t.id))

  def filter(condition: ErgoTransaction => Boolean): M

  /**
    * @return read-only copy of this history
    */
  def getReader: MempoolReader = this
}
