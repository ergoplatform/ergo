package scorex.testkit.properties.state

import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.testkit.{TestkitHelpers, generators}
import org.ergoplatform.testkit.generators.{CoreGenerators, SemanticallyInvalidModifierProducer}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait StateTests[ST <: ErgoState[ST]]
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreGenerators
    with TestkitHelpers
    with generators.SemanticallyValidModifierProducer[ST]
    with SemanticallyInvalidModifierProducer[ST] {

  val checksToMake = 10

  val stateGen: Gen[ST]
}
