package scorex.testkit.properties.state

import org.ergoplatform.nodeView.state.ErgoState
import scorex.testkit.{TestkitHelpers, generators}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait StateTests[ST <: ErgoState[ST]]
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TestkitHelpers
    with generators.SemanticallyValidModifierProducer[ST]
    with generators.SemanticallyInvalidModifierProducer[ST] {

  val checksToMake = 10

  val stateGen: Gen[ST]
}
