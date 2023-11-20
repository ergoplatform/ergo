package org.ergoplatform.testkit.properties.state

import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.testkit.TestkitHelpers
import org.ergoplatform.testkit.generators.{CoreGenerators, SemanticallyInvalidModifierProducer, SemanticallyValidModifierProducer}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.testkit.generators.SemanticallyValidModifierProducer

trait StateTests[ST <: ErgoState[ST]]
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreGenerators
    with TestkitHelpers
    with SemanticallyValidModifierProducer[ST]
    with SemanticallyInvalidModifierProducer[ST] {

  val checksToMake = 10

  val stateGen: Gen[ST]
}
