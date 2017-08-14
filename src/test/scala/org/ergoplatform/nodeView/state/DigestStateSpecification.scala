package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.testkit.TestkitHelpers


class DigestStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers {

  property("validate() - valid block") {

  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen){b =>
      val state = new DigestState(Array.fill(32)(0:Byte))
      state.validate(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid block") {

  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen){b =>
      val state = new DigestState(Array.fill(32)(0:Byte))
      state.applyModifier(b).isFailure shouldBe true
    }
  }
}
