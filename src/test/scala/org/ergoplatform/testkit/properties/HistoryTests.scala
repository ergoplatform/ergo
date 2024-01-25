package org.ergoplatform.testkit.properties

import org.ergoplatform.consensus.ModifierSemanticValidity.Valid
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.testkit.TestkitHelpers
import org.ergoplatform.testkit.generators.SyntacticallyTargetedModifierProducer
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.ScorexLogging


trait HistoryTests
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ScorexLogging
    with TestkitHelpers
    with SyntacticallyTargetedModifierProducer {

  val historyGen: Gen[ErgoHistory]

  lazy val generatorWithValidModifier: Gen[(ErgoHistory, BlockSection)] = {
    historyGen.map { h => (h, syntacticallyValidModifier(h))}
  }

  lazy val generatorWithInvalidModifier: Gen[(ErgoHistory, BlockSection)] = {
    historyGen.map { h => (h, syntacticallyInvalidModifier(h))}
  }

  private def propertyNameGenerator(propName: String): String = s"HistoryTests: $propName"

  property(propertyNameGenerator("applicable with valid modifier")) {
    forAll(generatorWithValidModifier) { case (h, m) => h.applicableTry(m) shouldBe 'success}
  }

  property(propertyNameGenerator("append valid modifier")) {
    forAll(generatorWithValidModifier) { case (h, m) => h.append(m).isSuccess shouldBe true }
  }

  property(propertyNameGenerator("contain valid modifier after appending")) {
    forAll(generatorWithValidModifier) { case (h, m) =>
      h.append(m)
      h.contains(m) shouldBe true
    }
  }

  property(propertyNameGenerator("find valid modifier after appending by modifierId")) {
    forAll(generatorWithValidModifier) { case (h, m) =>
      h.append(m)
      h.modifierById(m.id) shouldBe defined
    }
  }

  property(propertyNameGenerator("report semantically validation after appending valid modifier")) {
    forAll(generatorWithValidModifier) { case (h, m) =>
      h.append(m)
      h.reportModifierIsValid(m).get
      h.isSemanticallyValid(m.id) shouldBe Valid
    }
  }

  property(propertyNameGenerator("not applicable with invalid modifier")) {
    forAll(generatorWithInvalidModifier) { case (h, m) => h.applicableTry(m) shouldBe 'failure}
  }

  property(propertyNameGenerator("not append invalid modifier")) {
    forAll(generatorWithInvalidModifier) { case (h, m) => h.append(m).isSuccess shouldBe false }
  }

  property(propertyNameGenerator("not contain invalid modifier after appending")) {
    forAll(generatorWithInvalidModifier) { case (h, m) =>
      h.append(m)
      h.contains(m) shouldBe false
    }
  }

  property(propertyNameGenerator("not finds valid modifier after appending by modifierId")) {
    forAll(generatorWithInvalidModifier) { case (h, m) =>
      h.append(m)
      h.modifierById(m.id) shouldBe None
    }
  }
}
