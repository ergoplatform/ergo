package org.ergoplatform.mining

import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.http.api.ApiCodecs
import sigmastate.basics.DLogProtocol.ProveDlog


/**
  * Block candidate for external miner
  *
  * @param msg                            - message for external miner to work on
  * @param b                              - target value for mining
  * @param pk                             - public key of a miner
  * @param proofsForMandatoryTransactions - proofs of transactions membership (optional)
  *
  */
case class ExternalCandidateBlock(msg: Array[Byte],
                                  b: BigInt,
                                  pk: ProveDlog,
                                  proofsForMandatoryTransactions: Option[ProofOfUpcomingTransactions])

object ExternalCandidateBlock extends ApiCodecs {

  implicit val encoder: Encoder[ExternalCandidateBlock] = { c: ExternalCandidateBlock =>
    Json.obj(
      List(
        "msg" -> Some(c.msg.asJson),
        "b" -> Some(c.b.asJson(bigIntEncoder)),
        "pk" -> Some(c.pk.asJson),
        "proof" -> c.proofsForMandatoryTransactions.map(_.asJson)
      ).collect {
        //drop proof field if it is empty
        case (name, Some(value)) => name -> value
      }: _*)
  }

}
