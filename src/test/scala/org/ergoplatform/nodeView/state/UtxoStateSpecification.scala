package org.ergoplatform.nodeView.state

import java.io.File

import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.testkit.TestkitHelpers


class UtxoStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers {

  def withDir(dirName: String)(action: File => Any): Unit = {

    val dir = new File(dirName)
    dir.mkdirs()
    action(dir)
    dir.delete()
  }

  property("validate() - valid block") {

  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir("/tmp/utxotest2") { dir =>
        val state = new UtxoState(Array.fill(32)(0: Byte), dir)
        state.validate(b).isFailure shouldBe true
      }
    }
  }

  property("applyModifier() - valid block") {
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir("/tmp/utxotest4") { dir =>
        val state = new UtxoState(Array.fill(32)(0: Byte), dir)
        state.applyModifier(b).isFailure shouldBe true
      }
    }
  }
}

