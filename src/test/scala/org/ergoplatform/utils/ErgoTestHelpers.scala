package org.ergoplatform.utils

import java.net.InetSocketAddress
import java.util.concurrent.Executors

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.generators.ValidBlocksGenerators
import org.scalactic.{Prettifier, source}
import org.scalatest.enablers.{Collecting, InspectorAsserting}
import org.scalatest.{EitherValues, Inspectors, OptionValues}
import org.scalatest.{EitherValues, OptionValues, TryValues}
import scorex.core.network.{Incoming, Outgoing}
import scorex.core.network.peer.PeerInfo
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.util.ScorexLogging

import scala.collection.{GenMap, GenTraversable}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.higherKinds

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

  def inspectAll[E, C[_], A](xs: C[E])(fun: E => A)
                            (implicit collecting: Collecting[E, C[E]],
                             asserting: InspectorAsserting[A],
                             prettifier: Prettifier,
                             pos: source.Position): InspectorAsserting[A]#Result =
    Inspectors.forAll(xs)(fun)

  def inspectAll[K, V, MAP[k, v] <: GenMap[k, v], A](xs: MAP[K, V])(fun: ((K, V)) => A)
                                                    (implicit collecting: Collecting[(K, V), GenTraversable[(K, V)]],
                                                     asserting: InspectorAsserting[A],
                                                     prettifier: Prettifier,
                                                     pos: source.Position): InspectorAsserting[A]#Result =
    Inspectors.forAll(xs)(fun)

  val inetAddr1 = new InetSocketAddress("92.92.92.92", 27017)
  val inetAddr2 = new InetSocketAddress("93.93.93.93", 27017)
  val ts1 = System.currentTimeMillis() - 100
  val ts2 = System.currentTimeMillis() + 100

  val peers: Map[InetSocketAddress, PeerInfo] = Map(
    inetAddr1 -> PeerInfo(defaultPeerSpec.copy(nodeName = "first"), timeProvider.time()),
    inetAddr2 -> PeerInfo(defaultPeerSpec.copy(nodeName = "second"), timeProvider.time())
  )
}

object ErgoTestHelpers {

  implicit val defaultExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  val defaultTimeProvider: NetworkTimeProvider =
    new NetworkTimeProvider(ErgoSettings.read().scorexSettings.ntp)
}
