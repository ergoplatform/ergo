package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import io.circe.Encoder
import io.circe.Json
import org.bouncycastle.util.encoders.Hex
import org.ergoplatform.http.api.requests.MiningRequest
import org.ergoplatform.mining.CandidateGenerator.Candidate
import org.ergoplatform.mining.{AutolykosSolutionJsonCodecs, CandidateGenerator, ErgoMiner, WeakAutolykosSolution}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.{ErgoSettings, RESTApiSettings}
import org.ergoplatform.{AutolykosSolution, ErgoAddress, ErgoTreePredef, InputSolutionFound, OrderingSolutionFound, Pay2SAddress}
import scorex.core.api.http.ApiResponse
import sigma.data.ProveDlog
import sigma.serialization.GroupElementSerializer
import AutolykosSolutionJsonCodecs.jsonDecoder

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class MiningApiRoute(miner: ActorRef,
                          ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(ergoSettings.chainSettings).encoder

  override val route: Route = pathPrefix("mining") {
    candidateR ~
      candidateWithTxsR ~
      candidateWithTxsAndPkR ~
      solutionR ~
      weakSolutionR ~
      rewardAddressR ~
      rewardPublicKeyR
  }

  /**
    * Get block candidate. Useful for external miners.
    */
  def candidateR: Route = (path("candidate") & pathEndOrSingleSlash & get) {
    val prepareCmd = CandidateGenerator.GenerateCandidate(Seq.empty, reply = true, forced = false)
    val candidateF = miner.askWithStatus(prepareCmd).mapTo[Candidate].map(_.externalVersion)
    ApiResponse(candidateF)
  }

  /**
    * Get block candidate with transactions provided being included.
    * Useful for external miners when they want to insert certain transactions.
    */
  def candidateWithTxsR: Route = (path("candidateWithTxs")
    & post & entity(as[Seq[ErgoTransaction]]) & withAuth) { txs =>

    val prepareCmd = CandidateGenerator.GenerateCandidate(txs, reply = true, forced = false)
    val candidateF = miner.askWithStatus(prepareCmd).mapTo[Candidate].map(_.externalVersion)
    ApiResponse(candidateF)
  }

  def candidateWithTxsAndPkR: Route = (path("candidateWithTxsAndPk")
    & post & entity(as[MiningRequest]) & withAuth) { txsAndPk =>
    val tryPk = Try(GroupElementSerializer.fromBytes(Hex.decode(txsAndPk.pk)))
    val result = tryPk match {
      case Failure(_) =>
        Future.failed(new Exception("Could not decode hexadecimal string for given public key"))
      case Success(pk) =>
        val prepareCmd = CandidateGenerator.GenerateCandidate(txsAndPk.txs, reply = true,
          forced = false, Some(ProveDlog.apply(pk)))
        miner.askWithStatus(prepareCmd).mapTo[Candidate].map(_.externalVersion)
    }
    ApiResponse(result)
  }

  def solutionR: Route = (path("solution") & post & entity(as[AutolykosSolution])) { solution =>
    val result = if (ergoSettings.nodeSettings.useExternalMiner) {
      miner.askWithStatus(OrderingSolutionFound(solution)).mapTo[Unit]
    } else {
      Future.failed(new Exception("External miner support is inactive"))
    }
    ApiResponse(result)
  }

  def weakSolutionR: Route = (path("weakSolution") & post & entity(as[WeakAutolykosSolution])) { weakSolution =>
    val result = if (ergoSettings.nodeSettings.useExternalMiner) {
      val solution = new AutolykosSolution(weakSolution.pk, AutolykosSolution.wForV2, weakSolution.n, AutolykosSolution.dForV2)
      miner.askWithStatus(InputSolutionFound(solution)).mapTo[Unit]
    } else {
      Future.failed(new Exception("External miner support is inactive"))
    }
    ApiResponse(result)
  }

  def rewardAddressR: Route = (path("rewardAddress") & get) {
    val addressF: Future[ErgoAddress] =
      miner.askWithStatus(ErgoMiner.ReadMinerPk)
        .mapTo[ProveDlog]
        .map { pk =>
          val script = ErgoTreePredef.rewardOutputScript(ergoSettings.chainSettings.monetary.minerRewardDelay, pk)
          Pay2SAddress(script)(ergoSettings.addressEncoder)
        }

    ApiResponse(addressF.map(address => Json.obj("rewardAddress" -> address.asJson)))
  }

  def rewardPublicKeyR: Route = (path("rewardPublicKey") & get) {
    val pkF = miner.askWithStatus(ErgoMiner.ReadMinerPk).mapTo[ProveDlog]
    ApiResponse(pkF.map(pk => Json.obj("rewardPubkey" -> pk.asJson)))
  }

}
