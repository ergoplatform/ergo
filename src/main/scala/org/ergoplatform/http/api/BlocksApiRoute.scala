package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Route}
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, NonHeaderBlockSection}
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.{Algos, ErgoSettings, RESTApiSettings}
import org.ergoplatform.http.api.ApiError.BadRequest
import org.ergoplatform.nodeView.LocallyGeneratedBlockSection
import scorex.core.api.http.ApiResponse
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

import scala.concurrent.Future

case class BlocksApiRoute(viewHolderRef: ActorRef, readersHolder: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  // Limit for requests returning headers, to avoid too heavy requests
  private val MaxHeaders = 16384

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  val blocksPaging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 1, "limit".as[Int] ? 50)

  override val route: Route = pathPrefix("blocks") {
    getBlocksR ~
      postBlocksR ~
      getLastHeadersR ~
      getChainSliceR ~
      getBlockIdsAtHeightR ~
      getBlockHeaderByHeaderIdR ~
      getFullBlockByHeaderIdsR ~
      getBlockTransactionsByHeaderIdR ~
      getProofForTxR ~
      getFullBlockByHeaderIdR ~
      getModifierByIdR ~
      // input block related API
      getBestInputBlockR ~
      getBestInputBlocksChainR ~
      getInputBlockTransactionsR ~
      getInputBlockTransactionIdsR
  }

  private def getHistory: Future[ErgoHistoryReader] =
    (readersHolder ? GetDataFromHistory[ErgoHistoryReader](r => r)).mapTo[ErgoHistoryReader]

  private def getHeaderIdsAtHeight(h: Int): Future[Json] =
    getHistory.map { history =>
      history.headerIdsAtHeight(h).map(Algos.encode).asJson
    }

  private def getLastHeaders(n: Int): Future[Json] =
    getHistory.map { history =>
      history.lastHeaders(n).headers.map(_.asJson).asJson
    }

  private def getHeaderIds(offset: Int, limit: Int): Future[Json] =
    getHistory.map { history =>
      history.headerIdsAt(offset, limit).asJson
    }

  private def getFullBlockByHeaderId(headerId: ModifierId): Future[Option[ErgoFullBlock]] =
    getHistory.map { history =>
      history.typedModifierById[Header](headerId).flatMap(history.getFullBlock)
    }

  private def getFullBlockByHeaderIds(headerIds: Seq[ModifierId]): Future[Seq[ErgoFullBlock]] =
    getHistory.map { history =>
      headerIds.flatMap(headerId => history.typedModifierById[Header](headerId).flatMap(history.getFullBlock))
    }

  private def getModifierById(modifierId: ModifierId): Future[Option[BlockSection]] =
    getHistory.map(_.modifierById(modifierId))

  private def getProofForTx(headerId: ModifierId, txId: ModifierId): Future[Option[MerkleProof[Digest32]]] =
    getModifierById(headerId).flatMap {
      case Some(header: Header) =>
        val blockTxsId = NonHeaderBlockSection.computeId(
          BlockTransactions.modifierTypeId,
          headerId,
          header.transactionsRoot.asInstanceOf[Array[Byte]]
        )
        getModifierById(blockTxsId).map {
          case Some(txs: BlockTransactions) => txs.proofFor(txId)
          case _ => None
        }
      case _ => Future(None)
    }

  private def getChainSlice(fromHeight: Int, toHeight: Int): Future[Json] =
    getHistory.map { history =>
      val maxHeaderOpt = if (toHeight >= 0) {
        history.headerIdsAtHeight(toHeight)
          .headOption
          .flatMap(history.typedModifierById[Header](_))
          .orElse(history.bestHeaderOpt)
      } else {
        history.bestHeaderOpt
      }
      val headers = maxHeaderOpt
        .toIndexedSeq
        .flatMap { maxHeader =>
          history.headerChainBack(MaxHeaders, maxHeader, _.height <= fromHeight + 1).headers
        }
      headers.toList.asJson
    }

  private val chainPagination: Directive[(Int, Int)] =
    parameters("fromHeight".as[Int] ? 1, "toHeight".as[Int] ? MaxHeaders)

  def getBlocksR: Route = (pathEndOrSingleSlash & get & blocksPaging) { (offset, limit) =>
    if (offset < 0) {
      BadRequest("offset is negative")
    } else if (limit < 0) {
      BadRequest("limit is negative")
    } else if (limit > MaxHeaders) {
      BadRequest(s"No more than $MaxHeaders headers can be requested")
    } else {
      ApiResponse(getHeaderIds(offset, limit))
    }
  }

  def postBlocksR: Route = (post & entity(as[ErgoFullBlock])) { block =>
    if (ergoSettings.chainSettings.powScheme.validate(block.header).isSuccess) {
      log.info("Received a new valid block through the API: " + block)

      viewHolderRef ! LocallyGeneratedBlockSection(block.header)
      block.blockSections.foreach {
        viewHolderRef ! LocallyGeneratedBlockSection(_)
      }

      ApiResponse.OK
    } else {
      BadRequest("Block is invalid")
    }
  }

  def getChainSliceR: Route = (pathPrefix("chainSlice") & chainPagination) { (fromHeight, toHeight) =>
    if (toHeight < fromHeight) {
      BadRequest("toHeight < fromHeight")
    } else if (fromHeight - toHeight > MaxHeaders) {
      BadRequest(s"No more than $MaxHeaders headers can be requested")
    } else {
      ApiResponse(getChainSlice(fromHeight, toHeight))
    }
  }

  def getModifierByIdR: Route = (pathPrefix("modifier") & modifierId & get) { id =>
    ApiResponse(getModifierById(id))
  }

  def getProofForTxR: Route = (modifierId & pathPrefix("proofFor") & modifierId & get) { (headerId, txId) =>
    ApiResponse(getProofForTx(headerId, txId))
  }

  def getLastHeadersR: Route = (pathPrefix("lastHeaders" / IntNumber) & get) { count =>
    if (count > MaxHeaders) {
      BadRequest(s"No more than $MaxHeaders headers can be requested")
    } else {
      ApiResponse(getLastHeaders(count))
    }
  }

  def getBlockIdsAtHeightR: Route = (pathPrefix("at" / IntNumber) & get) { height =>
    ApiResponse(getHeaderIdsAtHeight(height))
  }

  def getBlockHeaderByHeaderIdR: Route = (modifierId & pathPrefix("header") & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id).map(_.map(_.header)))
  }

  def getBlockTransactionsByHeaderIdR: Route = (modifierId & pathPrefix("transactions") & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id).map(_.map(_.blockTransactions)))
  }

  def getFullBlockByHeaderIdR: Route = (modifierId & get) { id =>
    ApiResponse(getFullBlockByHeaderId(id))
  }

  def getFullBlockByHeaderIdsR: Route = (post & path("headerIds") & modifierIds) { ids =>
    ApiResponse(getFullBlockByHeaderIds(ids))
  }

  /**
    * Input/Ordering blocks related API methods
    */

  /**
    * @return ids of best ordering and input blocks
    */
  private def getBestInputBlockR = {
    (pathPrefix("bestInputBlock") & get) {
      ApiResponse(getHistory.map{ h =>
        val bh = h.bestHeaderOpt.map(_.id)
        val bi = h.bestInputBlock().map(_.id)
        Json.obj("bestOrdering" -> bh.getOrElse("").asJson, "bestInputBlock" -> bi.getOrElse("").asJson)
      })
    }
  }


  /**
    * @return ids of best input-blocks chain, along with ordering block id
    */
  private def getBestInputBlocksChainR = {
    (pathPrefix("bestInputChain") & get) {
      ApiResponse(getHistory.map{ h =>
        val bh = h.bestHeaderOpt.map(_.id)
        val bi = h.bestInputBlocksChain()
        Json.obj("bestOrdering" -> bh.getOrElse("").asJson, "bestInputBlocks" -> bi.asJson)
      })
    }
  }

  /**
    * @return transactions of input block with given id
    */
  private def getInputBlockTransactionsR = {
    (modifierId & pathPrefix("inputBlockTransactions") & get) { id =>
      ApiResponse {
        getHistory.map { history =>
          history.getInputBlockTransactions(id)
        }
      }
    }
  }

  /**
    * @return transaction ids of input block with given id
    */
  private def getInputBlockTransactionIdsR = {
    (modifierId & pathPrefix("inputBlockTransactionIds") & get) { id =>
      ApiResponse {
        getHistory.map { history =>
          history.getInputBlockTransactionIds(id)
        }
      }
    }
  }

}
