package org.ergoplatform.utils

import java.util.concurrent.Executors

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.generators.ValidBlocksGenerators
import org.scalactic.{Prettifier, source}
import org.scalatest.enablers.{Collecting, InspectorAsserting}
import org.scalatest.{EitherValues, Inspectors, OptionValues}
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.crypto.authds.ADDigest
import scorex.util.ScorexLogging
import scorex.util.encode.Base16

import scala.collection.{GenMap, GenTraversable}
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait ErgoTestHelpers
  extends ValidBlocksGenerators
    with ScorexLogging
    with ScorexEncoding
    with OptionValues
    with EitherValues {


  def updateHeight(box: ErgoBoxCandidate, creationHeight: Long): ErgoBoxCandidate =
    new ErgoBoxCandidate(box.value, box.proposition, box.additionalTokens, box.additionalRegisters, creationHeight)

  def changeValue(box: ErgoBoxCandidate, delta: Long): Option[ErgoBoxCandidate] = {
    if (-delta >= box.value) {
      None
    } else {
      Some(new ErgoBoxCandidate(box.value + delta, box.proposition, box.additionalTokens,
        box.additionalRegisters, box.creationHeight))
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
}

object ErgoTestHelpers {

  implicit val defaultExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  val defaultTimeProvider: NetworkTimeProvider =
    new NetworkTimeProvider(ErgoSettings.read(None).scorexSettings.ntp)
}
