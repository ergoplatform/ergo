package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.mining.{AutolykosSolution, ErgoMiner, ExternalCandidateBlock}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddress, ErgoScriptPredef, Pay2SAddress}
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.concurrent.Future

case class MiningApiRoute(miner: ActorRef,
                          ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(ergoSettings).encoder

  override val route: Route = pathPrefix("mining") {
    candidateR ~
      candidateWithTxsR ~
      solutionR ~
      rewardAddressR
  }

  /**
    * Get block candidate. Useful for external miners.
    */
  def candidateR: Route = (path("candidate") & pathEndOrSingleSlash & get) {
    val prepareCmd = ErgoMiner.PrepareCandidate(Seq.empty)
    val candidateF = (miner ? prepareCmd).mapTo[Future[ExternalCandidateBlock]].flatten
    ApiResponse(candidateF)
  }

  /**
    * Get block candidate with transactions provided being included.
    * Useful for external miners when they want to insert certain transactions.
    */
  def candidateWithTxsR: Route = (path("candidateWithTxs")
    & post & entity(as[Seq[ErgoTransaction]]) & withAuth) { txs =>

    val prepareCmd = ErgoMiner.PrepareCandidate(txs)
    val candidateF = (miner ? prepareCmd).mapTo[Future[ExternalCandidateBlock]].flatten
    ApiResponse(candidateF)
  }

  def solutionR: Route = (path("solution") & post & entity(as[AutolykosSolution])) { solution =>
    val result = if (ergoSettings.nodeSettings.useExternalMiner) {
      (miner ? solution).mapTo[Future[Unit]].flatten
    } else {
      Future.failed(new Exception("External miner support is inactive"))
    }
    ApiResponse(result)
  }

  def rewardAddressR: Route = (path("rewardAddress") & get) {
    val addressF = (miner ? ErgoMiner.ReadMinerPk).mapTo[Option[ProveDlog]]
      .flatMap {
        _.fold[Future[ErgoAddress]](Future.failed(new Exception("Miner PK not initialized")))(pk => {
          val script = ErgoScriptPredef.rewardOutputScript(ergoSettings.chainSettings.monetary.minerRewardDelay, pk)
          Future.successful(Pay2SAddress(script)(ergoSettings.addressEncoder))
        })
      }
    ApiResponse(addressF.map(address => Json.obj("rewardAddress" -> address.asJson)))
  }

}
