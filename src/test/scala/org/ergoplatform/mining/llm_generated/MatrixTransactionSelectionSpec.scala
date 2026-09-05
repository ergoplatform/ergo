package org.ergoplatform.mining

import com.google.common.io.Files.createTempDir
import org.ergoplatform.{DataInput, ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCompilerHelpers, ErgoCorePropertyTest}
import org.ergoplatform.utils.ErgoCoreTestConstants.{defaultMinerPk, emptyVSUpdate, parameters}
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.interpreter.ProverResult

class MatrixTransactionSelectionSpec extends ErgoCorePropertyTest with ErgoCompilerHelpers {
  private val orderingTree = compileSourceV5("CONTEXT.minerPubKey.size >= 0", 0)

  private def box(index: Byte, tree: ErgoTree = TrueTree): ErgoBox = new ErgoBox(
    value = 1000000000L, ergoTree = tree, creationHeight = 0,
    additionalTokens = Colls.emptyColl, additionalRegisters = Map.empty,
    transactionId = bytesToId(Array.fill(32)(index)), index = 0
  )

  private def spend(input: ErgoBox, outputTree: ErgoTree = TrueTree,
                    dataInputs: IndexedSeq[DataInput] = IndexedSeq.empty): ErgoTransaction =
    ErgoTransaction(IndexedSeq(Input(input.id, ProverResult.empty)), dataInputs,
      IndexedSeq(new ErgoBoxCandidate(input.value, outputTree, 1)))

  private def select(boxes: Seq[ErgoBox], txs: Seq[ErgoTransaction]) = {
    val state = UtxoState.fromBoxHolder(BoxHolder(boxes), None, createTempDir(), settings, parameters)
    val context = state.stateContext.upcoming(defaultMinerPk.value, 1L, settings.chainSettings.initialNBits,
      Array.emptyByteArray, emptyVSUpdate, 4.toByte)
    val result = CandidateGenerator.collectTxs(defaultMinerPk, parameters.maxBlockCost,
      parameters.maxBlockSize, state, context, txs)
    // Both selected payloads must have a UTXO proof independently of the other new payload.
    Seq(result._1, result._2).filter(_.nonEmpty).foreach { payload =>
      state.proofsForTransactions(payload).isSuccess shouldBe true
    }
    result
  }

  property("ordinary and soft-field transactions enter their respective partitions") {
    val inputBox = box(1)
    val orderingBox = box(2, orderingTree)
    val inputTx = spend(inputBox)
    val orderingTx = spend(orderingBox)
    val (input, ordering, invalid) = select(Seq(inputBox, orderingBox), Seq(inputTx, orderingTx))
    input shouldBe Seq(inputTx)
    ordering shouldBe Seq(orderingTx)
    invalid shouldBe empty
  }

  property("input-only dependency chains remain in the input payload") {
    val initial = box(3)
    val parent = spend(initial)
    val child = spend(parent.outputs.head)
    val (input, ordering, invalid) = select(Seq(initial), Seq(parent, child))
    input shouldBe Seq(parent, child)
    ordering shouldBe empty
    invalid shouldBe empty
  }

  property("ordering-only dependency chains remain in the ordering payload") {
    val initial = box(4, orderingTree)
    val parent = spend(initial, orderingTree)
    val child = spend(parent.outputs.head)
    val (input, ordering, invalid) = select(Seq(initial), Seq(parent, child))
    input shouldBe empty
    ordering shouldBe Seq(parent, child)
    invalid shouldBe empty
  }

  property("cross-partition spending dependencies and descendants are deferred without elimination") {
    Seq(false, true).foreach { orderingParent =>
      val initial = box(5, if (orderingParent) orderingTree else TrueTree)
      val parent = spend(initial, if (orderingParent) TrueTree else orderingTree)
      val child = spend(parent.outputs.head)
      val grandchild = spend(child.outputs.head)
      val independentBox = box(10)
      val independent = spend(independentBox)
      val (input, ordering, invalid) = select(Seq(initial, independentBox), Seq(parent, child, grandchild, independent))
      (input ++ ordering) should contain theSameElementsAs Seq(parent, independent)
      invalid shouldBe empty
    }
  }

  property("cross-partition data dependencies are deferred in either direction") {
    Seq(false, true).foreach { orderingParent =>
      val first = box(6, if (orderingParent) orderingTree else TrueTree)
      val second = box(7, if (orderingParent) TrueTree else orderingTree)
      val parent = spend(first)
      val child = spend(second, dataInputs = IndexedSeq(DataInput(parent.outputs.head.id)))
      val (input, ordering, invalid) = select(Seq(first, second), Seq(parent, child))
      (input ++ ordering) shouldBe Seq(parent)
      invalid shouldBe empty
    }
  }

  property("same-partition data dependencies remain selectable") {
    val first = box(8)
    val second = box(9)
    val parent = spend(first)
    val child = spend(second, dataInputs = IndexedSeq(DataInput(parent.outputs.head.id)))
    val (input, ordering, invalid) = select(Seq(first, second), Seq(parent, child))
    input shouldBe Seq(parent, child)
    ordering shouldBe empty
    invalid shouldBe empty
  }
}
