package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoGenerators
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.testkit.TestkitHelpers


class UtxoStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers {

  property("validate()") {

  }

  property("applyModifier()") {

  }
}

