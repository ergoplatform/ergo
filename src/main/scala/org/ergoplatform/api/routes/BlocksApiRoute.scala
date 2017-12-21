package org.ergoplatform.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{CandidateBlock, Header}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.ModifierId
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object BlocksApiRoute {

  case class MinedBlock(candidate: CandidateBlock, nonce: Long)

}

case class BlocksApiRoute(nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings, nodeId: Array[Byte], digest: Boolean)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ScorexLogging {

  import BlocksApiRoute._

  private val powScheme = ergoSettings.chainSettings.poWScheme

  override val route: Route = pathPrefix("blocks") {
    concat(getBlocksR, postBlocksR, getLastHeadersR, getBlockIdsAtHeightR, getBlockHeaderByHeaderIdR, getBlockTransactionsByHeaderIdR, candidateBlockR, getFullBlockByHeaderIdR)
  }

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private val request = if (digest) {
    GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, ErgoHistory](_.history)
  } else {
    GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, ErgoHistory](_.history)
  }

  private def getHistory = (nodeViewActorRef ? request).mapTo[ErgoHistory]

  private def getHeaderIdsAtHeight(h: Int): Future[Json] = getHistory.map {
    _.headerIdsAtHeight(h)
  }.map {
    headerIds =>
      headerIds.map(Base58.encode).asJson
  }

  private def getLastHeaders(n: Int): Future[Json] = getHistory.map {
    _.lastHeaders(n)
  }.map { v =>
    v.headers.map(_.json).asJson
  }

  private def getHeaderIds(limit: Int, offset: Int): Future[Json] = {
    getHistory.map {
      _.lastHeaders(limit, offset).headers.map(_.id)
    }.map { v =>
      v.map(Base58.encode).asJson
    }
  }

  private def getFullBlockByHeaderId(headerId: ModifierId): Future[Option[ErgoFullBlock]] = {
    getHistory.map { h =>
      h.typedModifierById[Header](headerId).flatMap(h.getFullBlock)
    }
  }

  def getBlocksR: Route = get {
    parameters('limit.as[Int] ? 50, 'offset.as[Int] ? 0, 'heightFrom.as[Int].?, 'heightTo.as[Int].?) {
      case (limit, offset, heightFrom, heightTo) =>
        // todo heightFrom, heightTo
        toJsonResponse(getHeaderIds(limit, offset))
    }
  }

  def postBlocksR: Route = post {
    entity(as[MinedBlock]) { block =>
      complete {
        powScheme.proveBlock(block.candidate, block.nonce) match {
          case Some(newBlock) =>
            log.info("New block found: " + newBlock)

            nodeViewActorRef ! LocallyGeneratedModifier(newBlock.header)
            nodeViewActorRef ! LocallyGeneratedModifier(newBlock.blockTransactions)
            newBlock.aDProofs.foreach { adp =>
              nodeViewActorRef ! LocallyGeneratedModifier(adp)
            }
            StatusCodes.OK
          case None =>
            StatusCodes.BadRequest
        }
      }
    }
  }

  def getLastHeadersR: Route =
    path("lastHeaders" / Segment) { countString =>
      get {
        toJsonResponse {
          Try {
            countString.toInt
          } match {
            case Success(count) =>
              getLastHeaders(count)
            case Failure(e) =>
              Future.failed(e)
          }
        }
      }
    }

  // todo: heightString validation
  def getBlockIdsAtHeightR: Route = path("at" / Segment) { heightString =>
    get {
      toJsonResponse {
        Try {
          heightString.toInt
        } match {
          case Success(height) =>
            getHeaderIdsAtHeight(height)
          case Failure(e) =>
            Future.failed(e)
        }
      }
    }
  }

  // todo: headerId validation
  def getBlockHeaderByHeaderIdR: Route = path(Segment / "header") { headerId =>
    get {
      toJsonOptionalResponse {
        getFullBlockByHeaderId(ModifierId @@ Base58.decode(headerId).get)
          .map(_.map(_.header.json))
      }
    }
  }

  // todo: headerId validation
  def getBlockTransactionsByHeaderIdR: Route = path(Segment / "transactions") { headerId =>
    get {
      toJsonOptionalResponse {
        getFullBlockByHeaderId(ModifierId @@ Base58.decode(headerId).get)
          .map(_.map(_.transactions.map(_.json).asJson))
      }
    }
  }

  def candidateBlockR: Route = path("candidateBlock") {
    get {
      toJsonOptionalResponse {
        ErgoMiner.produceCandidate(nodeViewActorRef, ergoSettings, nodeId)
          .map(_.map(_.json))
      }
    }
  }

  // todo: headerId validation
  def getFullBlockByHeaderIdR: Route = path(Segment) { headerId =>
    get {
      toJsonOptionalResponse {
        getFullBlockByHeaderId(ModifierId @@ Base58.decode(headerId).get)
          .map(_.map(_.json))
      }
    }
  }
}
