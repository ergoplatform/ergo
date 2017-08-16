package org.ergoplatform.mining.difficulty

import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.duration._
import scala.util.Try

class LinearDifficultyControlSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators {

  val Epoch = 123
  val control = new LinearDifficultyControl(1.minute, Epoch)
  val UseLastEpochs = control.UseLastEpochs

  property("previousHeadersRequiredForRecalculation() should return correct heights required for recalculation") {
    control.previousHeadersRequiredForRecalculation(Epoch * (UseLastEpochs + 1)) shouldEqual
      Seq(Epoch * (UseLastEpochs + 1) - Epoch / 2, Epoch * (UseLastEpochs + 1) - 3 * Epoch / 2,
        Epoch * (UseLastEpochs + 1) - 5 * Epoch / 2, Epoch * (UseLastEpochs + 1) - 7 * Epoch / 2)
  }

  property("previousHeadersRequiredForRecalculation() should return previous block if there should not be difficulty recalculation") {
    control.previousHeadersRequiredForRecalculation(Epoch / 2) shouldBe Seq(Epoch / 2 - 1)
    control.previousHeadersRequiredForRecalculation(Epoch) shouldBe Seq(Epoch - 1)
    control.previousHeadersRequiredForRecalculation(Epoch * UseLastEpochs) shouldBe Seq(Epoch * UseLastEpochs - 1)
    control.previousHeadersRequiredForRecalculation(Epoch * UseLastEpochs * 2 + 1) shouldBe
      Seq(Epoch * UseLastEpochs * 2)
  }

  property("calculate() should require correct heights") {
    val diff = Constants.InitialDifficulty
    val previousDifficulties = (0 until UseLastEpochs * Epoch by Epoch).map(i => (i, diff))
    Try(control.calculate(previousDifficulties)) shouldBe 'success
    Try(control.calculate(previousDifficulties.map(i => (i._1 * 2, i._2)))) shouldBe 'failure
  }


}
