package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.WrappedUtxoState
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.VersionTag


class UtxoStateSpecification extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {

  property("fromBoxHolder") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.take(1000)._1.foreach { box =>
        us.boxById(box.id) shouldBe Some(box)
      }
    }
  }

  property("fromBoxHolder  + proofsForTransactions") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      val boxes = bh.take(1000)._1
      val tx = AnyoneCanSpendTransaction(boxes.map(_.nonce).toIndexedSeq, IndexedSeq(boxes.map(_.value).sum))
      us.proofsForTransactions(Seq(tx)).isSuccess shouldBe true
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

  property("checkTransactions()") {
    forAll(boxesHolderGen) { bh =>
      val txs = validTransactionsFromBoxHolder(bh)._1

      val boxIds = txs.flatMap(_.boxIdsToOpen)
      boxIds.foreach(id => assert(bh.get(ByteArrayWrapper(id)).isDefined))
      assert(boxIds.distinct.size == boxIds.size)

      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))
      val digest = us.proofsForTransactions(txs).get._2
      us.checkTransactions(txs, digest).isSuccess shouldBe true
    }
  }

  property("applyModifier() - valid full block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)
      us.applyModifier(block).isSuccess shouldBe true
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createUtxoState
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("2 forks switching") {
    val (us, bh) = ErgoState.generateGenesisUtxoState(createTempDir, None)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get
    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)
    val wusChain1Block1 = wusAfterGenesis.applyModifier(chain1block1).get
    val chain1block2 = validFullBlock(Some(chain1block1.header), wusChain1Block1)

    val (us2, bh2) = ErgoState.generateGenesisUtxoState(createTempDir, None)
    val wus2AfterGenesis = WrappedUtxoState(us2, bh2, None).applyModifier(genesis).get
    val chain2block1 = validFullBlock(Some(genesis.header), wus2AfterGenesis)
    val wusChain2Block1 = wus2AfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    var (state, _) = ErgoState.generateGenesisUtxoState(createTempDir, None)
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
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))
        val genesis = validFullBlock(parentOpt = None, us, bh)
        val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get
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