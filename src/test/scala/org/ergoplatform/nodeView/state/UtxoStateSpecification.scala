package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}


class UtxoStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {

  property("fromBoxHolder") {
    forAll(boxesStorageGen){ bh =>
      withDir(s"/tmp/utxotest-${bh.hashCode()}") { dir =>
        val us = UtxoState.fromBoxHolder(bh, dir)
        bh.take(1000)._1.foreach {box =>
          us.boxById(box.id) shouldBe Some(box)
        }
      }
    }
  }

  property("validate() - valid block") {

  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir("/tmp/utxotest3") { dir =>
        val state = new UtxoState(dir)
        state.validate(b).isFailure shouldBe true
      }
    }
  }

  property("applyModifier() - valid block") {
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir("/tmp/utxotest5") { dir =>
        val state = new UtxoState(dir)
        state.applyModifier(b).isFailure shouldBe true
      }
    }
  }
}