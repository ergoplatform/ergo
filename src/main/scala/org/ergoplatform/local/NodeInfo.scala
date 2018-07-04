package org.ergoplatform.local

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, ApiSettings}

case class NodeInfo(nodeName: String,
                    appVersion: String,
                    unconfirmedCount: Int,
                    peersCount: Int,
                    stateRoot: Option[String],
                    stateType: StateType,
                    stateVersion: Option[String],
                    isMining: Boolean,
                    bestHeaderOpt: Option[Header],
                    headersScore: Option[BigInt],
                    bestFullBlockOpt: Option[ErgoFullBlock],
                    fullBlocksScore: Option[BigInt],
                    launchTime: Long) {
}

class NodeInfoEncoder(implicit val settings: ApiSettings) extends ApiCodecs with Encoder[NodeInfo] {

  def apply(ni: NodeInfo): Json = {
    Map(
      "name" -> ni.nodeName.asJson,
      "appVersion" -> Version.VersionString.asJson,
      "headersHeight" -> ni.bestHeaderOpt.map(_.height).asJson,
      "fullHeight" -> ni.bestFullBlockOpt.map(_.header.height).asJson,
      "bestHeaderId" -> ni.bestHeaderOpt.map(_.encodedId).asJson,
      "bestFullHeaderId" -> ni.bestFullBlockOpt.map(_.header.encodedId).asJson,
      "previousFullHeaderId" -> ni.bestFullBlockOpt.map(_.header.parentId).map(Algos.encode).asJson,
      "difficulty" -> ni.bestFullBlockOpt.map(_.header.requiredDifficulty).map(difficultyEncoder.apply).asJson,
      "headersScore" -> ni.headersScore.map(difficultyEncoder.apply).asJson,
      "fullBlocksScore" -> ni.fullBlocksScore.map(difficultyEncoder.apply).asJson,
      "unconfirmedCount" -> ni.unconfirmedCount.asJson,
      "stateRoot" -> ni.stateRoot.asJson,
      "stateType" -> ni.stateType.stateTypeName.asJson,
      "stateVersion" -> ni.stateVersion.asJson,
      "isMining" -> ni.isMining.asJson,
      "peersCount" -> ni.peersCount.asJson,
      "launchTime" -> ni.launchTime.asJson
    ).asJson
  }
}
