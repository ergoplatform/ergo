package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}


class DigestStateSpecification extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {


  property("validate() - valid block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.rootHash)
      ds.validate(block).get
    }
  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(Array.fill(32)(0: Byte))
      state.validate(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.rootHash)
      ds.applyModifier(block).isSuccess shouldBe true
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(Array.fill(32)(0: Byte))
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("rollback & rollback versions") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.rootHash)

      ds.rollbackVersions.size shouldEqual 1

      val ds2 = ds.applyModifier(block).get

      ds2.rollbackVersions.size shouldEqual 2

      ds2.rootHash.sameElements(ds.rootHash) shouldBe false

      val ds3 = ds2.rollbackTo(ds.rootHash).get
      ds3.rootHash shouldBe ds.rootHash

      ds3.applyModifier(block).get.rootHash shouldBe ds2.rootHash
    }
  }
}
