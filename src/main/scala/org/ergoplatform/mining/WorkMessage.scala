package org.ergoplatform.mining

import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.block.Block.Version
import sigmastate.basics.DLogProtocol.ProveDlog


/**
  * Block candidate related data for external miner to perform work
  *
  * @param msg                            - message for external miner to work on (serialized header)
  * @param b                              - target value for mining
  * @param h                              - height of the block
  * @param pk                             - public key of a miner
  * @param proofsForMandatoryTransactions - proofs of transactions membership (optional)
  *
  */
case class WorkMessage(msg: Array[Byte],
                       b: BigInt,
                       h: Height,
                       pk: ProveDlog,
                       proofsForMandatoryTransactions: Option[ProofOfUpcomingTransactions])

object WorkMessage extends ApiCodecs {

  implicit val encoder: Encoder[WorkMessage] = { workMessage: WorkMessage =>
    Json.obj(
      List(
        "msg" -> Some(workMessage.msg.asJson),
        "b" -> Some(workMessage.b.asJson(bigIntEncoder)),
        "h" -> Some(workMessage.h.asJson),
        "pk" -> Some(workMessage.pk.asJson),
        "proof" -> workMessage.proofsForMandatoryTransactions.map(_.asJson)
      ).collect {
        //drop proof field if it is empty
        case (name, Some(value)) => name -> value
      }: _*)
  }

}
