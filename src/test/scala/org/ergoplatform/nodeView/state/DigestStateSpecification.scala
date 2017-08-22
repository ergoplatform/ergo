package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

import scala.util.Random


class DigestStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {

  property("validate() - valid block") {
    forAll(boxesHolderGen) { bh =>
      with2Dirs(s"/tmp/digest-${bh.hashCode()}", s"/tmp/digest-2-${bh.hashCode()}") { case (dir1, dir2) =>

        val us = UtxoState.fromBoxHolder(bh, dir1)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

        val parent = ErgoFullBlock.genesisWithStateDigest(us.rootHash).header
        val block = validFullBlock(parent, us, bh)

        val ds = DigestState.create(us.rootHash, dir2).get
        ds.validate(block).get
      }
    }
  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir(s"/tmp/digest-${Random.nextInt()}") {dir =>
        val state = DigestState.create(Array.fill(32)(0: Byte), dir).get
        state.validate(b).isFailure shouldBe true
      }
    }
  }

  property("applyModifier() - valid block") {
    forAll(boxesHolderGen) { bh =>
      with2Dirs(s"/tmp/digest-${bh.hashCode()}", s"/tmp/digest-2-${bh.hashCode()}") { case (dir1, dir2) =>

        val us = UtxoState.fromBoxHolder(bh, dir1)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

        val parent = ErgoFullBlock.genesisWithStateDigest(us.rootHash).header
        val block = validFullBlock(parent, us, bh)

        val ds = DigestState.create(us.rootHash, dir2).get
        ds.applyModifier(block).isSuccess shouldBe true
      }
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir(s"/tmp/digest-${Random.nextInt()}") { dir =>
        val state = DigestState.create(Array.fill(32)(0: Byte), dir).get
        state.applyModifier(b).isFailure shouldBe true
      }
    }
  }

  property("rollback & rollback versions") {
    forAll(boxesHolderGen) { bh =>
      with2Dirs(s"/tmp/digest-${bh.hashCode()}", s"/tmp/digest-2-${bh.hashCode()}") { case (dir1, dir2) =>
        val us = UtxoState.fromBoxHolder(bh, dir1)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

        val parent = ErgoFullBlock.genesisWithStateDigest(us.rootHash).header
        val block = validFullBlock(parent, us, bh)

        val ds = DigestState.create(us.rootHash, dir2).get

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
}
