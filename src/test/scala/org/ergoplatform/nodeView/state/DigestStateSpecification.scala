package org.ergoplatform.nodeView.state

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.VersionTag
import scorex.crypto.authds.ADDigest

class DigestStateSpecification extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {

  private val emptyVersion: VersionTag = VersionTag @@ Array.fill(32)(0: Byte)
  private val emptyAdDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  property("reopen") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val fb = validFullBlock(parentOpt = None, us, bh)
      val dir2 = createTempDir
      val ds = DigestState.create(Some(us.version), Some(us.rootHash), dir2, ErgoSettings.read(None).nodeSettings).get
      ds.applyModifier(fb).isSuccess shouldBe true
      ds.close()

      val state = DigestState.create(None, None, dir2, ErgoSettings.read(None).nodeSettings).get
      state.version shouldEqual fb.header.id
      state.rootHash shouldEqual fb.header.stateRoot
    }
  }

  property("validate() - valid block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.version, us.rootHash)
      ds.validate(block).get
    }
  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(emptyVersion, emptyAdDigest)
      state.validate(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.version, us.rootHash)
      ds.applyModifier(block).isSuccess shouldBe true
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(emptyVersion, emptyAdDigest)
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("rollback & rollback versions") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.version, us.rootHash)

      ds.rollbackVersions.size shouldEqual 1

      val ds2 = ds.applyModifier(block).get

      ds2.rollbackVersions.size shouldEqual 2

      ds2.rootHash.sameElements(ds.rootHash) shouldBe false

      val ds3 = ds2.rollbackTo(ds.version).get
      ds3.rootHash shouldBe ds.rootHash

      ds3.applyModifier(block).get.rootHash shouldBe ds2.rootHash
    }
  }
}
