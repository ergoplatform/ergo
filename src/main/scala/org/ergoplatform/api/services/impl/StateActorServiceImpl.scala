package org.ergoplatform.api.services.impl

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.api.services.StateService
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.VersionTag

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

class StateActorServiceImpl[ST <: ErgoState[ST]: ClassTag](stateActorRef: ActorRef)(implicit val ec: ExecutionContext) extends StateService {

  implicit val timeout = Timeout(5.seconds)

  private val request = GetDataFromCurrentView[ErgoHistory, ST, ErgoWallet, ErgoMemPool, ST](_.state)

  def getVersion: Future[VersionTag] = (stateActorRef ? request).mapTo[ST].map(_.version)

  def getType: Future[String] = (stateActorRef ? request).mapTo[ST].map{
    case _: DigestState => "digest"
    case _: UtxoState => "utxo"
    case _ => "unknown"
  }

}
