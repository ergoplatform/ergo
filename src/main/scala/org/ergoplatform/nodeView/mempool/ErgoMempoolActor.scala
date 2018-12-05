package org.ergoplatform.nodeView.mempool

import akka.actor.Actor
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMempoolActor.{MempoolRequest, MempoolResponse}
import org.ergoplatform.settings.MemoryPoolSettings
import scorex.core.NodeViewComponent.MempoolComponent
import scorex.core.NodeViewComponentOperation.GetReader
import scorex.core.transaction.MempoolOperation._
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

/**
  *
  * TODO This is simplified implementation of Memory pool.
  * MemPool should:
  * - have limited size
  * - replace transactions with the lowest fee if size limit is reached
  * - validate transactions when put (is called)
  * - clean transactions, that become invalid
  *
  * @param unconfirmed transaction storage, should not be accessed outside of this class, all updates
  *                    should be synchronized with correspondent `txQueue` field updates
  */
class ErgoMempoolActor private[mempool](unconfirmed: TrieMap[ModifierId, ErgoTransaction], settings: MemoryPoolSettings)
  extends Actor with ScorexLogging {

  /** same transactions as in `unconfirmed` field transaction ordered by their fee
    */
  private var txQueue = mutable.PriorityQueue[ErgoTransaction](unconfirmed.values.toSeq: _*)(Ordering.by(fee))

  /** Read-only copy of this history
    */
  private val reader = new ErgoMempoolReader(unconfirmed)

  def receive: Receive = put orElse remove orElse getReader

  protected def put: Receive = {
    case put: Put[ErgoTransaction] if put.mode == PutWithoutCheck =>
      putToPool(put.transaction)

    case put: Put[ErgoTransaction] =>
      val added = putToPool(put.transaction)
      val result = if (added) {
        Success(())
      } else {
        Failure(new NoSuchElementException(s"Duplicate transaction ${put.transaction}"))
      }
      sender ! PutResponse(put.transaction, result, put.mode)
  }

  def putToPool(tx: ErgoTransaction): Boolean = {
    val shouldAdd = !unconfirmed.contains(tx.id)
    if (shouldAdd) {
      unconfirmed.put(tx.id, tx)
      completeAssembly(tx)
      txQueue.enqueue(tx)
      fixSizeIfNeeded()
    }
    shouldAdd
  }

  def remove: Receive = {
    case msg: Remove[ErgoTransaction] => deleteFromPool(msg.transaction)
    case filter: FilterBy[ErgoTransaction] => filterFromPool(filter.criteria)
  }

  def deleteFromPool(tx: ErgoTransaction): Unit = {
    if (unconfirmed.contains(tx.id)) {
      unconfirmed.remove(tx.id)
      txQueue = txQueue.filter(_.id != tx.id)
    }
  }

  def filterFromPool(condition: ErgoTransaction => Boolean): Unit = {
    unconfirmed.retain { case (_, v) =>
      condition(v)
    }
    txQueue = txQueue.filter(condition)
  }

  def maxPoolSize: Option[Int] = {
    if (settings.maxPoolSize > 0) Some(settings.maxPoolSize) else None
  }

  /** Read-only copy of this history
    */
  def getReader: Receive = {
    case GetReader(MempoolComponent) => sender ! reader
  }

  private def fixSizeIfNeeded(): Unit = {
    if (maxPoolSize.exists(maximum => unconfirmed.size > maximum)) {
      log.warn(s"Memory pool size ${unconfirmed.size} exceeds the maximum allowed ${settings.maxPoolSize}")

    }
  }

  def fee(tx: ErgoTransaction): Long = {
    0
  }

  /** Map stores current state of waiting for query building
    * - value - promise of result and set of all transactions of request
    * - key - set of transactions that are waiting for the assembly
    */
  //TODO: Cover with tests and refactor to  {{ case class ResponsePromise }}
  private[mempool] var waitingForAssembly: Map[Set[ModifierId], (Promise[MempoolResponse], Seq[ModifierId])] = Map.empty

  //TODO rework option.get
  protected def completeAssembly(tx: ErgoTransaction): Unit = synchronized {
    val newMap = waitingForAssembly.flatMap { case (waitedTxIds, entry) =>
      val newKey = waitedTxIds - tx.id
      // filtering fully-built queries and completing of a promise
      if (newKey.isEmpty) {
        val (promise, allIds) = entry
        promise complete Success(allIds.map(unconfirmed))
        None
      } else {
        Some(newKey -> entry)
      }
    }
    waitingForAssembly = newMap
  }

  def waitForAll(ids: MempoolRequest): Future[MempoolResponse] = synchronized {
    val promise = Promise[Seq[ErgoTransaction]]
    waitingForAssembly = waitingForAssembly.updated(ids.toSet, (promise, ids))
    promise.future
  }

}


object ErgoMempoolActor {

  type MempoolRequest = Seq[ModifierId]

  type MempoolResponse = Seq[ErgoTransaction]

  def empty(memoryPoolSettings: MemoryPoolSettings): ErgoMemPool = new ErgoMemPool(TrieMap.empty, memoryPoolSettings)

}
