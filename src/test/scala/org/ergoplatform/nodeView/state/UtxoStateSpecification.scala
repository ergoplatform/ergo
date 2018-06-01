package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.nodeView.WrappedUtxoState
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import scorex.core.VersionTag
import sigmastate.Values.TrueLeaf

import scala.util.Random


class UtxoStateSpecification extends ErgoPropertyTest {

  property("extractEmissionBox() should extract correct box") {
    val (us, bh) = createUtxoState()
    forAll { seed: Int =>
      val fb = validFullBlock(None, us, bh, new Random(seed))
      us.extractEmissionBox(fb) should not be None
    }
  }

  property("extractEmissionBox() should not extract fake box") {
    var (us, bh) = createUtxoState()
    val t = validTransactionsFromBoxHolder(bh)
    val txs = t._1
    bh = t._2

    // first block that spends and creates emission box
    val fb = validFullBlock(None, us, txs)
    val newEmissionBox = us.extractEmissionBox(fb)
    newEmissionBox should not be None
    us = us.applyModifier(fb).get

    // second block, that do not contain emission box
    val fb2 = validFullBlock(None, us, bh)
    us.extractEmissionBox(fb2) shouldBe None

    // third block, that do contain box similar to emission one, but with lower amount
    val ft = fb2.blockTransactions.txs.head
    val fakeCandidate: ErgoBoxCandidate = new ErgoBoxCandidate(newEmissionBox.get.value + 1,
      newEmissionBox.get.proposition,
      newEmissionBox.get.additionalRegisters)
    val txs3 = fb2.blockTransactions.txs.tail :+ ft.copy(outputCandidates = ft.outputCandidates.tail :+ fakeCandidate)
    val fb3 = fb2.copy(blockTransactions = fb2.blockTransactions.copy(txs = txs3))
    us.extractEmissionBox(fb3) shouldBe None
  }

  property("fromBoxHolder") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.take(1000)._1.foreach { box =>
        us.boxById(box.id) shouldBe Some(box)
      }
    }
  }

  property("proofsForTransactions") {
    var (us: UtxoState, bh) = createUtxoState()
    var height: Int = 0
    forAll(invalidHeaderGen) { header =>
      val t = validTransactionsFromBoxHolder(bh, new Random(height))
      val txs = t._1
      bh = t._2
      val (adProofBytes, adDigest) = us.proofsForTransactions(txs).get
      val realHeader = header.copy(stateRoot = adDigest, ADProofsRoot = ADProofs.proofDigest(adProofBytes), height = height)
      val adProofs = ADProofs(realHeader.id, adProofBytes)
      val fb = ErgoFullBlock(realHeader, BlockTransactions(realHeader.id, txs), Some(adProofs))
      us = us.applyModifier(fb).get
      height = height + 1
    }
  }

  property("proofsForTransactions() to be deterministic") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      val txs = validTransactionsFromBoxHolder(bh)._1

      val (proof1, digest1) = us.proofsForTransactions(txs).get
      val (proof2, digest2) = us.proofsForTransactions(txs).get

      ADProofs.proofDigest(proof1) shouldBe ADProofs.proofDigest(proof2)
      digest1 shouldBe digest2
    }
  }

  property("applyTransactions()") {
    forAll(boxesHolderGen) { bh =>
      val txs = validTransactionsFromBoxHolder(bh)._1

      val created = txs.flatMap(_.outputs.map(_.id)).map(ByteArrayWrapper.apply)
      val boxIds = txs.flatMap(_.inputs.map(_.boxId)).map(ByteArrayWrapper.apply)
      boxIds.distinct.size shouldBe boxIds.size
      val toRemove = boxIds.filterNot(id => created.contains(id))
      toRemove.foreach(id => bh.get(id) should not be None)

      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)
      val digest = us.proofsForTransactions(txs).get._2
      us.applyTransactions(txs, digest, height = 1).get
    }
  }

  property("applyModifier() for real genesis state") {
    var (us: UtxoState, bh) = createUtxoState()
    var height = 0
    forAll(invalidHeaderGen) { header =>
      val t = validTransactionsFromBoxHolder(bh, new Random(12))
      val txs = t._1
      bh = t._2
      val (adProofBytes, adDigest) = us.proofsForTransactions(txs).get
      val realHeader = header.copy(stateRoot = adDigest, ADProofsRoot = ADProofs.proofDigest(adProofBytes), height = height)
      val adProofs = ADProofs(realHeader.id, adProofBytes)
      val fb = ErgoFullBlock(realHeader, BlockTransactions(realHeader.id, txs), Some(adProofs))
      us = us.applyModifier(fb).get
      height = height + 1
    }
  }

  property("applyModifier() - valid full block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)
      us.applyModifier(block).get
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createUtxoState()._1
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid full block after invalid one") {
    val (us, bh) = createUtxoState()
    val validBlock = validFullBlock(parentOpt = None, us, bh)

    //Different state
    val (us2, bh2) = {
      lazy val genesisSeed = Long.MaxValue
      lazy val rndGen = new scala.util.Random(genesisSeed)

      lazy val initialBoxes: Seq[ErgoBox] =
        (1 to 1).map(_ => ErgoBox(value = 10000, TrueLeaf))

      val bh = BoxHolder(initialBoxes)

      createUtxoState(bh) -> bh
    }
    val invalidBlock = validFullBlock(parentOpt = None, us2, bh2)

    us.applyModifier(invalidBlock).isSuccess shouldBe false
    us.applyModifier(validBlock).isSuccess shouldBe true
  }


  property("2 forks switching") {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)
    val wusChain1Block1 = wusAfterGenesis.applyModifier(chain1block1).get
    val chain1block2 = validFullBlock(Some(chain1block1.header), wusChain1Block1)

    val (us2, bh2) = createUtxoState()
    val wus2AfterGenesis = WrappedUtxoState(us2, bh2, stateConstants).applyModifier(genesis).get
    val chain2block1 = validFullBlock(Some(genesis.header), wus2AfterGenesis)
    val wusChain2Block1 = wus2AfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    var (state, _) = createUtxoState()
    state = state.applyModifier(genesis).get

    state = state.applyModifier(chain1block1).get

    state = state.rollbackTo(VersionTag @@ genesis.id).get
    state = state.applyModifier(chain2block1).get
    state = state.applyModifier(chain2block2).get

    state = state.rollbackTo(VersionTag @@ genesis.id).get
    state = state.applyModifier(chain1block1).get
    state = state.applyModifier(chain1block2).get

  }

  property("rollback n blocks and apply again") {
    forAll(boxesHolderGen, smallPositiveInt) { (bh, depth) =>
      whenever(depth > 0 && depth <= 5) {
        val us = createUtxoState(bh)
        bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)
        val genesis = validFullBlock(parentOpt = None, us, bh)
        val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
        wusAfterGenesis.rootHash shouldEqual genesis.header.stateRoot

        val (finalState: WrappedUtxoState, chain: Seq[ErgoFullBlock]) = (0 until depth)
          .foldLeft((wusAfterGenesis, Seq(genesis))) { (sb, i) =>
            val state = sb._1
            val block = validFullBlock(parentOpt = Some(sb._2.last.header), state)
            (state.applyModifier(block).get, sb._2 ++ Seq(block))
          }
        val finalRoot = finalState.rootHash
        finalRoot shouldEqual chain.last.header.stateRoot

        val rollbackedState = finalState.rollbackTo(VersionTag @@ genesis.id).get
        rollbackedState.rootHash shouldEqual genesis.header.stateRoot

        val finalState2: WrappedUtxoState = chain.tail.foldLeft(rollbackedState) { (state, block) =>
          state.applyModifier(block).get
        }

        finalState2.rootHash shouldEqual finalRoot
      }
    }
  }


}