package org.ergoplatform.local

import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.state.StateType
import scorex.crypto.encode.Base58

case class NodeInfo(nodeName: String,
                    appVersion: String,
                    unconfirmedCount: Int,
                    peersCount: Int,
                    stateRoot: String,
                    stateType: StateType,
                    stateVersion: String,
                    isMining: Boolean,
                    votes: String,
                    bestHeaderOpt: Option[Header],
                    bestFullBlockOpt: Option[ErgoFullBlock]) {
  lazy val json = Map(
    "name" -> nodeName.asJson,
    "appVersion" -> Version.VersionString.asJson,
    "headersHeight" -> bestHeaderOpt.map(_.height).getOrElse(-1).asJson,
    "fullHeight" -> bestFullBlockOpt.map(_.header.height).getOrElse(-1).asJson,
    "bestHeaderId" -> bestHeaderOpt.map(_.encodedId).getOrElse("null").asJson,
    "bestFullHeaderId" -> bestFullBlockOpt.map(_.header.encodedId).getOrElse("null").asJson,
    "previousFullHeaderId" -> bestFullBlockOpt.map(_.header.parentId).map(Base58.encode).getOrElse("null").asJson,
    "difficulty" -> bestFullBlockOpt.map(_.header.requiredDifficulty).getOrElse(BigInt(0)).toString(10).asJson,
    "unconfirmedCount" -> unconfirmedCount.asJson,
    "stateRoot" -> stateRoot.asJson,
    "stateType" -> stateType.stateTypeName.asJson,
    "stateVersion" -> stateVersion.asJson,
    "isMining" -> isMining.asJson,
    "votes" -> votes.asJson,
    "peersCount" -> peersCount.asJson
  ).asJson
}
