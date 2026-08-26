package org.ergoplatform.utils

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.{InputBlockFound, InputBlockHeaderFound, NothingFound, OrderingBlockFound, OrderingBlockHeaderFound, ProveBlockResult}
import org.scalatest.{EitherValues, OptionValues}
import org.ergoplatform.network.peer.PeerInfo
import scorex.util.ScorexLogging

import java.net.InetSocketAddress

trait ErgoTestHelpers
  extends ScorexLogging
    with ScorexEncoding
    with OptionValues
    with EitherValues {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  def await[A](f: scala.concurrent.Future[A]): A = scala.concurrent.Await.result[A](f, defaultAwaitDuration)

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

  /**
    * Extracts a Header from ProveBlockResult, handling all possible outcomes.
    * Throws RuntimeException if no valid PoW solution is found.
    */
  def extractHeaderFromProveResult(result: ProveBlockResult): Header = result match {
    case InputBlockHeaderFound(h) => h
    case OrderingBlockHeaderFound(h) => h
    case InputBlockFound(fb) => fb.header
    case OrderingBlockFound(fb) => fb.header
    case NothingFound => throw new RuntimeException("No valid PoW found")
  }

  /**
    * Extracts an ErgoFullBlock from ProveBlockResult, handling all possible outcomes.
    * For header-only results, throws an exception as full block data is not available.
    * Throws RuntimeException if no valid PoW solution is found.
    */
  def extractFullBlockFromProveResult(result: ProveBlockResult): ErgoFullBlock = result match {
    case InputBlockFound(fb) => fb
    case OrderingBlockFound(fb) => fb
    case InputBlockHeaderFound(_) =>
      throw new RuntimeException("Expected full block but got header-only result (InputBlockHeaderFound)")
    case OrderingBlockHeaderFound(_) =>
      throw new RuntimeException("Expected full block but got header-only result (OrderingBlockHeaderFound)")
    case NothingFound => throw new RuntimeException("No valid PoW found")
  }
}

object ErgoTestHelpers {

  implicit val defaultExecutionContext: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.fromExecutor(java.util.concurrent.Executors.newFixedThreadPool(10))
}
