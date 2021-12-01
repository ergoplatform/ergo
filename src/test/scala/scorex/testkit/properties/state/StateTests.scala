package scorex.testkit.properties.state

import org.ergoplatform.nodeView.state.ErgoState
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.{CoreGenerators, SemanticallyInvalidModifierProducer, SemanticallyValidModifierProducer}

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
