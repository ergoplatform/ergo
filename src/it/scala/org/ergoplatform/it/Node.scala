package org.ergoplatform.it

import com.typesafe.config.Config
import org.asynchttpclient._
import org.ergoplatform.it.api.{NetworkNodeApi, NodeApi}
import org.ergoplatform.settings.ErgoSettings
import org.slf4j.LoggerFactory

import scala.concurrent.duration.FiniteDuration


class Node(config: Config, val nodeInfo: NodeInfo, override val client: AsyncHttpClient)
  extends NodeApi with NetworkNodeApi {
  val privateKey: String = config.getString("private-key")
  val publicKey: String = config.getString("public-key")
  val address: String = config.getString("address")
  val accountSeed: String = config.getString("account-seed")
  val settings: ErgoSettings = ErgoSettings.fromConfig(config)

  override protected val log = LoggerFactory.getLogger(s"${getClass.getName}.${settings.scorexSettings.network.nodeName}")

  override val chainId: Char = 'I'
  override val nodeName: String = s"it-test-client-to-${nodeInfo.networkIpAddress}"
  override val restAddress: String = "localhost"
  override val networkAddress: String = "localhost"
  override val nodeRestPort: Int = nodeInfo.hostRestApiPort
  override val matcherRestPort: Int = nodeInfo.hostMatcherApiPort
  override val networkPort: Int = nodeInfo.hostNetworkPort
  override val blockDelay: FiniteDuration = settings.chainSettings.blockInterval
}
