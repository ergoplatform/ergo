package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}


class DigestStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {

  property("validate() - valid block") {
    forAll(boxesHolderGen) { bh =>
      withDir(s"/tmp/digest-test-${bh.hashCode()}}") { dir =>

        val us = UtxoState.fromBoxHolder(bh, dir)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

        val parent = ErgoFullBlock.genesisWithStateDigest(us.rootHash).header
        val block = validFullBlock(parent, us, bh)

        val ds = new DigestState(us.rootHash)
        ds.validate(block).get
      }
    }
  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen){b =>
      val state = new DigestState(Array.fill(32)(0:Byte))
      state.validate(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid block") {
    forAll(boxesHolderGen) { bh =>
      withDir(s"/tmp/digest-test-${bh.hashCode()}}") { dir =>

        val us = UtxoState.fromBoxHolder(bh, dir)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

        val parent = ErgoFullBlock.genesisWithStateDigest(us.rootHash).header
        val block = validFullBlock(parent, us, bh)

        val ds = new DigestState(us.rootHash)
        ds.applyModifier(block).isSuccess shouldBe true
      }
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen){b =>
      val state = new DigestState(Array.fill(32)(0:Byte))
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("rollback") {
    //todo: implement
  }
}
