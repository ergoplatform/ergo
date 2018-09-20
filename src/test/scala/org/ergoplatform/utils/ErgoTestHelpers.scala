package org.ergoplatform.utils

import java.net.InetSocketAddress
import java.util.concurrent.Executors

import org.ergoplatform.settings.ErgoSettings
import org.scalatest.{EitherValues, OptionValues, TryValues}
import scorex.core.network.{Incoming, Outgoing}
import scorex.core.network.peer.PeerInfo
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext

trait ErgoTestHelpers
  extends ValidBlocksGenerators
    with ScorexLogging
    with ScorexEncoding
    with OptionValues
    with TryValues
    with EitherValues {

  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider


  val inetAddr1 = new InetSocketAddress("92.92.92.92", 27017)
  val inetAddr2 = new InetSocketAddress("93.93.93.93", 27017)
  val ts1 = System.currentTimeMillis() - 100
  val ts2 = System.currentTimeMillis() + 100

  val peers = Map(
    inetAddr1 -> PeerInfo(ts1, Some(inetAddr1), Some("first"), Some(Outgoing), Seq()),
    inetAddr2 -> PeerInfo(ts2, None, Some("second"), Some(Incoming), Seq())
  )
}

object ErgoTestHelpers {

  implicit val defaultExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  val defaultTimeProvider: NetworkTimeProvider = new NetworkTimeProvider(ErgoSettings.read(None).scorexSettings.ntp)
}
