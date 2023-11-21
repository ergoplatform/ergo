package org.ergoplatform.utils

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.utils.generators.ValidBlocksGenerators
import org.scalatest.{EitherValues, OptionValues}
import org.ergoplatform.network.peer.PeerInfo
import scorex.util.ScorexLogging

import java.net.InetSocketAddress
import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}

trait ErgoTestHelpers
  extends ValidBlocksGenerators
    with ScorexLogging
    with ScorexEncoding
    with OptionValues
    with EitherValues {

  def await[A](f: Future[A]): A = Await.result[A](f, defaultAwaitDuration)

  def updateHeight(box: ErgoBoxCandidate, creationHeight: Int): ErgoBoxCandidate =
    new ErgoBoxCandidate(box.value, box.ergoTree, creationHeight, box.additionalTokens, box.additionalRegisters)

  def changeValue(box: ErgoBoxCandidate, delta: Long): Option[ErgoBoxCandidate] = {
    if (-delta >= box.value) {
      None
    } else {
      Some(new ErgoBoxCandidate(Math.addExact(box.value, delta), box.ergoTree, box.creationHeight,
        box.additionalTokens, box.additionalRegisters))
    }
  }

  val inetAddr1 = new InetSocketAddress("92.92.92.92", 27017)
  val inetAddr2 = new InetSocketAddress("93.93.93.93", 27017)

  val peers: Map[InetSocketAddress, PeerInfo] = Map(
    inetAddr1 -> PeerInfo(defaultPeerSpec.copy(nodeName = "first"), System.currentTimeMillis()),
    inetAddr2 -> PeerInfo(defaultPeerSpec.copy(nodeName = "second"), System.currentTimeMillis())
  )
}

object ErgoTestHelpers {

  implicit val defaultExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
}
