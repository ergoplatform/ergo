package org.ergoplatform.api.services.impl

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.api.services.HistoryService
import org.ergoplatform.modifiers.history.{Header, HeaderChain}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.NodeViewHolder.GetDataFromCurrentView

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class HistoryActorServiceImpl[ST <: ErgoState[ST]](historyActorRef: ActorRef)(implicit val ec: ExecutionContext)
  extends HistoryService {

  implicit val timeout = Timeout(5.seconds)

  private val request = GetDataFromCurrentView[ErgoHistory, ST, ErgoWallet, ErgoMemPool, ErgoHistory](_.history)

  private def getHistory: Future[ErgoHistory] =
    (historyActorRef ? request).mapTo[ErgoHistory]

  def getHeight: Future[Int] = getHistory.map(_.height)

  def getBestHeader: Future[Option[Header]] = getHistory.map(_.bestHeaderOpt)

  def getBestFullBlock: Future[Option[ErgoFullBlock]] = getHistory.map(_.bestFullBlockOpt)

  def getLastHeaders(n: Int): Future[HeaderChain] = getHistory.map(_.lastHeaders(n))

  def getModifierById(id: String): Future[Option[ErgoPersistentModifier]] = getHistory.map(_.modifierById(id))

  def getCurrentDifficulty: Future[BigInt] = getHistory.map(_.requiredDifficulty)
}
