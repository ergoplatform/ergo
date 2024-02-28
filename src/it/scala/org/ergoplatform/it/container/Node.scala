package org.ergoplatform.it.container

import org.asynchttpclient._
import org.ergoplatform.it.api.{NetworkNodeApi, NodeApi}
import org.ergoplatform.settings.ErgoSettings
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration


class Node(val settings: ErgoSettings, val nodeInfo: NodeInfo, override val client: AsyncHttpClient)
          (implicit override val ec: ExecutionContext) extends NodeApi with NetworkNodeApi {
// todo after addresses will added
//  val privateKey: String = config.getString("private-key")
//  val publicKey: String = config.getString("public-key")
//  val address: String = config.getString("address")
//  val accountSeed: String = config.getString("account-seed")

  override protected val log: Logger =
    LoggerFactory.getLogger(s"${getClass.getName}.${settings.scorexSettings.network.nodeName}")

  def nodeName: String = settings.scorexSettings.network.nodeName
  def containerId: String = nodeInfo.containerId
  override val chainId: Char = 'I'
  override val networkNodeName: String = s"it-test-client-to-${nodeInfo.networkIpAddress}"
  override val restAddress: String = nodeInfo.apiIpAddress
  override val networkAddress: String = "localhost"
  override val nodeRestPort: Int = nodeInfo.containerApiPort
  override val networkPort: Int = nodeInfo.hostNetworkPort
  override val blockDelay: FiniteDuration = settings.chainSettings.blockInterval

}
