package org.ergoplatform.utils

import java.util.concurrent.Executors

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.generators.ValidBlocksGenerators
import org.scalactic.{Prettifier, source}
import org.scalatest.enablers.{Collecting, InspectorAsserting}
import org.scalatest.{EitherValues, Inspectors, OptionValues, TryValues}
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.util.ScorexLogging

import scala.collection.{GenMap, GenTraversable}
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait ErgoTestHelpers
  extends ValidBlocksGenerators
    with ScorexLogging
    with ScorexEncoding
    with OptionValues
    with EitherValues {

  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider

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
