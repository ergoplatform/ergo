package org.ergoplatform.mining

import com.google.common.io.Files.createTempDir
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
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

class MatrixFeeSelectionSpec extends ErgoCorePropertyTest with ErgoCompilerHelpers {
  private val fee = 1000000L
  private val delay = settings.chainSettings.monetary.minerRewardDelay
  private val feeTree = ErgoTreePredef.feeProposition(delay)
  private val rewardTree = ErgoTreePredef.rewardOutputScript(delay, defaultMinerPk)
  private val orderingTree = compileSourceV5("CONTEXT.minerPubKey.size >= 0", 0)

  private def box(index: Byte, tree: ErgoTree = TrueTree): ErgoBox = new ErgoBox(
    value = 1000000000L, ergoTree = tree, creationHeight = 0,
    additionalTokens = Colls.emptyColl, additionalRegisters = Map.empty,
    transactionId = bytesToId(Array.fill(32)(index)), index = 0
  )

  private def payment(source: ErgoBox): ErgoTransaction = ErgoTransaction(
    IndexedSeq(Input(source.id, ProverResult.empty)), IndexedSeq.empty,
    IndexedSeq(
      new ErgoBoxCandidate(source.value - fee, TrueTree, 1),
      new ErgoBoxCandidate(fee, feeTree, 1)
    )
  )

  private def state(boxes: Seq[ErgoBox], protocolCost: Int = parameters.maxBlockCost): UtxoState =
    UtxoState.fromBoxHolder(BoxHolder(boxes), None, createTempDir(), settings, parameters.withBlockCost(protocolCost))

  private def select(boxes: Seq[ErgoBox], transactions: Seq[ErgoTransaction], version: Byte,
                     prefix: Seq[ErgoTransaction] = Seq.empty,
                     maxCost: Int = parameters.maxBlockCost,
                     maxSize: Int = parameters.maxBlockSize,
                     protocolCost: Int = parameters.maxBlockCost) = {
    val us = state(boxes, protocolCost)
    val context = us.stateContext.upcoming(defaultMinerPk.value, 1L,
      settings.chainSettings.initialNBits, Array.emptyByteArray, emptyVSUpdate, version)
    val result = CandidateGenerator.collectTxs(defaultMinerPk, maxCost,
      maxSize, us, context, transactions, prefix)
    Seq(prefix ++ result._1, prefix ++ result._2).filter(_.nonEmpty).foreach { payload =>
      us.proofsForTransactions(payload).isSuccess shouldBe true
      payload.foldLeft(Vector.empty[ErgoTransaction]) { (applied, tx) =>
        us.withTransactions(applied).validateWithCost(tx, context, protocolCost,
          None, softFieldsAllowed = true).isSuccess shouldBe true
        applied :+ tx
      }
    }
    result
  }

  private def payloadCost(boxes: Seq[ErgoBox], txs: Seq[ErgoTransaction], version: Byte): Int = {
    val us = state(boxes, 10000000)
    val context = us.stateContext.upcoming(defaultMinerPk.value, 1L,
      settings.chainSettings.initialNBits, Array.emptyByteArray, emptyVSUpdate, version)
    val (_, cost) = txs.foldLeft((Vector.empty[ErgoTransaction], 0)) { case ((applied, total), tx) =>
      val txCost = us.withTransactions(applied).validateWithCost(tx, context, 10000000,
        None, softFieldsAllowed = true).get
      (applied :+ tx, total + txCost)
    }
    cost
  }

  private def sectionSize(txs: Seq[ErgoTransaction]): Int =
    BlockTransactions(bytesToId(Array.fill[Byte](32)(0)), Header.Interpreter60Version, txs).size

  private def transactionCost(boxes: Seq[ErgoBox], tx: ErgoTransaction,
                              prefix: Seq[ErgoTransaction] = Seq.empty): Int = {
    val us = state(boxes)
    val context = us.stateContext.upcoming(defaultMinerPk.value, 1L,
      settings.chainSettings.initialNBits, Array.emptyByteArray, emptyVSUpdate, Header.Interpreter60Version)
    us.withTransactions(prefix).validateWithCost(tx, context, parameters.maxBlockCost,
      None, softFieldsAllowed = true).get
  }

  private def assertFeeReward(reward: ErgoTransaction, payments: Seq[ErgoTransaction]): Unit = {
    reward.inputs.map(in => bytesToId(in.boxId)) should contain theSameElementsAs
      payments.map(tx => bytesToId(tx.outputs.last.id))
    reward.dataInputs shouldBe empty
    reward.outputs should have size 1
    val output = reward.outputs.head
    output.value shouldBe payments.size.toLong * fee
    output.value should be > 0L
    output.propositionBytes.toSeq shouldBe rewardTree.bytes.toSeq
    output.creationHeight shouldBe 1
    output.additionalTokens.length shouldBe 0
  }

  Seq(Header.InitialVersion, Header.HardeningVersion, Header.Interpreter50Version).foreach { version =>
    property(s"version $version collects the payment fee into the exact delayed miner reward") {
      val source = box(1)
      val tx = payment(source)
      val (input, ordering, invalid) = select(Seq(source), Seq(tx), version)
      input shouldBe empty
      invalid shouldBe empty
      ordering should have size 2
      ordering.head.id shouldBe tx.id
      assertFeeReward(ordering.last, Seq(tx))
    }
  }

  property("version 4 collects an ordering payment fee in its ordering payload") {
    val source = box(2, orderingTree)
    val tx = payment(source)
    val (input, ordering, invalid) = select(Seq(source), Seq(tx), Header.Interpreter60Version)
    input shouldBe empty
    invalid shouldBe empty
    ordering should have size 2
    ordering.head.id shouldBe tx.id
    assertFeeReward(ordering.last, Seq(tx))
  }

  property("version 4 leaves a new input payment fee out of the alternative ordering payload") {
    val source = box(3)
    val tx = payment(source)
    val (input, ordering, invalid) = select(Seq(source), Seq(tx), Header.Interpreter60Version)
    input.map(_.id) shouldBe Seq(tx.id)
    ordering shouldBe empty
    invalid shouldBe empty
  }

  property("independent mixed payments collect only the ordering source fee") {
    val inputSource = box(4)
    val orderingSource = box(5, orderingTree)
    val inputTx = payment(inputSource)
    val orderingTx = payment(orderingSource)
    val (input, ordering, invalid) = select(Seq(inputSource, orderingSource),
      Seq(inputTx, orderingTx), Header.Interpreter60Version)
    input.map(_.id) shouldBe Seq(inputTx.id)
    invalid shouldBe empty
    ordering should have size 2
    ordering.head.id shouldBe orderingTx.id
    assertFeeReward(ordering.last, Seq(orderingTx))
  }

  property("an accepted input payment produces its fee reward with an empty new pool") {
    val source = box(10)
    val accepted = payment(source)
    val (input, ordering, invalid) = select(Seq(source), Seq.empty,
      Header.Interpreter60Version, prefix = Seq(accepted))
    input shouldBe empty
    invalid shouldBe empty
    ordering should have size 1
    assertFeeReward(ordering.head, Seq(accepted))
  }

  property("an accepted input payment duplicated in the pool is skipped without invalidation") {
    val source = box(11)
    val accepted = payment(source)
    val (input, ordering, invalid) = select(Seq(source), Seq(accepted),
      Header.Interpreter60Version, prefix = Seq(accepted))
    input shouldBe empty
    invalid shouldBe empty
    ordering should have size 1
    ordering.map(_.id) should not contain accepted.id
    assertFeeReward(ordering.head, Seq(accepted))
  }

  property("a descendant of an accepted input payment uses the accepted prefix state") {
    val source = box(12)
    val accepted = payment(source)
    val child = payment(accepted.outputs.head)
    val (input, ordering, invalid) = select(Seq(source), Seq(child),
      Header.Interpreter60Version, prefix = Seq(accepted))
    input.map(_.id) shouldBe Seq(child.id)
    invalid shouldBe empty
    ordering should have size 1
    assertFeeReward(ordering.head, Seq(accepted))
  }

  property("new input fees remain excluded while accepted input fees are collected") {
    val acceptedSource = box(13)
    val newSource = box(14)
    val accepted = payment(acceptedSource)
    val fresh = payment(newSource)
    val (input, ordering, invalid) = select(Seq(acceptedSource, newSource), Seq(fresh),
      Header.Interpreter60Version, prefix = Seq(accepted))
    input.map(_.id) shouldBe Seq(fresh.id)
    invalid shouldBe empty
    ordering should have size 1
    assertFeeReward(ordering.head, Seq(accepted))
  }

  property("an ordering payment is not admitted when its fee reward exceeds the size budget") {
    val source = box(15, orderingTree)
    val tx = payment(source)
    val (_, complete, _) = select(Seq(source), Seq(tx), Header.Interpreter60Version)
    complete should have size 2
    val budget = sectionSize(Seq(tx))
    sectionSize(complete) should be > budget
    val (input, ordering, invalid) = select(Seq(source), Seq(tx), Header.Interpreter60Version,
      maxSize = budget)
    input shouldBe empty
    ordering shouldBe empty
    invalid shouldBe empty
  }

  property("an ordering payment is not admitted when its fee reward exceeds the cost budget") {
    val source = box(16, orderingTree)
    val tx = payment(source)
    val (_, complete, _) = select(Seq(source), Seq(tx), Header.Interpreter60Version)
    complete should have size 2
    val budget = transactionCost(Seq(source), tx)
    budget should be > 0
    transactionCost(Seq(source), complete.last, Seq(tx)) should be > 0
    val (input, ordering, invalid) = select(Seq(source), Seq(tx), Header.Interpreter60Version,
      maxCost = budget)
    input shouldBe empty
    ordering shouldBe empty
    invalid shouldBe empty
  }

  Seq("size", "cost").foreach { limit =>
    property(s"an accepted prefix remains valid with its fee unspent when the reward exceeds the $limit budget") {
      val source = box(17)
      val accepted = payment(source)
      val (_, complete, _) = select(Seq(source), Seq.empty, Header.Interpreter60Version,
        prefix = Seq(accepted))
      complete should have size 1
      val prefixSize = sectionSize(Seq(accepted))
      sectionSize(Seq(accepted) ++ complete) should be > prefixSize
      val prefixCost = transactionCost(Seq(source), accepted)
      transactionCost(Seq(source), complete.head, Seq(accepted)) should be > 0
      val (input, ordering, invalid) = select(Seq(source), Seq.empty, Header.Interpreter60Version,
        prefix = Seq(accepted),
        maxCost = if (limit == "cost") prefixCost else parameters.maxBlockCost,
        maxSize = if (limit == "size") prefixSize else parameters.maxBlockSize)
      input shouldBe empty
      ordering shouldBe empty
      invalid shouldBe empty
      val unspent = org.ergoplatform.nodeView.state.ErgoState.newBoxes(Seq(accepted) ++ ordering)
      unspent.map(b => bytesToId(b.id)) should contain(bytesToId(accepted.outputs.last.id))
    }
  }

  Seq(true, false).foreach { fits =>
    property(s"new input growth is ${if (fits) "accepted" else "deferred"} according to the cumulative conservative cost allowance") {
      val sources = Seq(box(18), box(19))
      val txs = sources.map { source =>
        ErgoTransaction(IndexedSeq(Input(source.id, ProverResult.empty)), IndexedSeq.empty,
          IndexedSeq(new ErgoBoxCandidate(source.value, TrueTree, 1)))
      }
      val costs = txs.map(tx => transactionCost(sources, tx))
      costs.foreach(_ should be > 0)
      val total = costs.sum
      total should be < parameters.maxBlockCost
      val allowance = if (fits) total + 100 else costs.max + (total - costs.max) / 2
      allowance should be > costs.max
      allowance should be < parameters.maxBlockCost
      if (fits) total should be <= allowance else total should be > allowance
      val (input, ordering, invalid) = select(sources, Seq(txs.last),
        Header.Interpreter60Version, prefix = Seq(txs.head), maxCost = allowance)
      input.map(_.id) shouldBe (if (fits) Seq(txs.last.id) else Seq.empty)
      ordering shouldBe empty
      invalid shouldBe empty
    }
  }

  property("a mandatory prefix above the conservative allowance is retained without additional input growth") {
    val sources = Seq(box(22), box(23), box(24))
    val txs = sources.map { source =>
      ErgoTransaction(IndexedSeq(Input(source.id, ProverResult.empty)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(source.value, TrueTree, 1)))
    }
    val costs = txs.map(tx => transactionCost(sources, tx))
    costs.foreach(_ should be > 0)
    val prefixCost = costs.take(2).sum
    val allowance = costs.max + (prefixCost - costs.max) / 2
    allowance should be > costs.max
    prefixCost should be > allowance
    costs.sum should be < parameters.maxBlockCost
    val (input, ordering, invalid) = select(sources, Seq(txs.last),
      Header.Interpreter60Version, prefix = txs.take(2), maxCost = allowance)
    input shouldBe empty
    ordering shouldBe empty
    invalid shouldBe empty
    val retained = org.ergoplatform.nodeView.state.ErgoState.newBoxes(txs.take(2) ++ ordering)
    retained.map(b => bytesToId(b.id)).toSet shouldBe
      txs.take(2).flatMap(_.outputs).map(b => bytesToId(b.id)).toSet
  }

  property("a new input payment is deferred when its accepted prefix fills the ordering size budget") {
    val acceptedSource = box(20)
    val newSource = box(21)
    val accepted = payment(acceptedSource)
    val fresh = payment(newSource)
    val budget = math.max(sectionSize(Seq(accepted)), sectionSize(Seq(fresh)))
    sectionSize(Seq(accepted, fresh)) should be > budget
    val (input, ordering, invalid) = select(Seq(acceptedSource, newSource), Seq(fresh),
      Header.Interpreter60Version, prefix = Seq(accepted), maxSize = budget)
    input shouldBe empty
    ordering shouldBe empty
    invalid shouldBe empty
    val unspent = org.ergoplatform.nodeView.state.ErgoState.newBoxes(Seq(accepted) ++ ordering)
    unspent.map(b => bytesToId(b.id)) should contain(bytesToId(accepted.outputs.last.id))
  }

  for {
    version <- Seq(Header.InitialVersion, Header.Interpreter50Version, Header.Interpreter60Version)
    count <- Seq(127, 128)
  } {
    property(s"version $version matches serialized section capacity for $count transactions") {
      val sources = (0 until count).map(i => box(i.toByte, orderingTree))
      val transactions = sources.map { source =>
        ErgoTransaction(IndexedSeq(Input(source.id, ProverResult.empty)), IndexedSeq.empty,
          IndexedSeq(new ErgoBoxCandidate(source.value, TrueTree, 1)))
      }
      def serializedSize(txs: Seq[ErgoTransaction]): Int =
        BlockTransactions(bytesToId(Array.fill[Byte](32)(0)), version, txs).size
      val totalCost = payloadCost(sources, transactions, version)
      totalCost should be > parameters.maxBlockCost
      val testProtocolCost = totalCost + 100000
      testProtocolCost should be > totalCost
      val exactSize = serializedSize(transactions)
      val (input, ordering, invalid) = select(sources, transactions, version, maxSize = exactSize,
        maxCost = testProtocolCost, protocolCost = testProtocolCost)
      input shouldBe empty
      invalid shouldBe empty
      ordering.map(_.id) shouldBe transactions.map(_.id)
      serializedSize(ordering) shouldBe exactSize

      val (shortInput, shortOrdering, shortInvalid) =
        select(sources, transactions, version, maxSize = exactSize - 1,
          maxCost = testProtocolCost, protocolCost = testProtocolCost)
      shortInput shouldBe empty
      shortInvalid shouldBe empty
      shortOrdering.map(_.id) shouldBe transactions.dropRight(1).map(_.id)
      serializedSize(shortOrdering) should be <= (exactSize - 1)
    }
  }

  property("cumulative prefix and new input cost cannot exceed the consensus ordering budget") {
    val sources = Seq(box(30), box(31))
    val txs = sources.map { source =>
      ErgoTransaction(IndexedSeq(Input(source.id, ProverResult.empty)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(source.value, TrueTree, 1)))
    }
    val individualCosts = txs.map(tx => payloadCost(sources, Seq(tx), Header.Interpreter60Version))
    individualCosts.foreach(_ should be > 0)
    // Exact measured cost leaves only a rounded JIT allowance after initialization.
    // Give each script headroom while keeping the combined payload over budget.
    val consensusBudget = individualCosts.max + (individualCosts.sum - individualCosts.max) / 2
    consensusBudget should be > individualCosts.max
    individualCosts.sum should be > consensusBudget
    val restrictedState = state(sources, consensusBudget)
    val restrictedContext = restrictedState.stateContext.upcoming(defaultMinerPk.value, 1L,
      settings.chainSettings.initialNBits, Array.emptyByteArray, emptyVSUpdate, Header.Interpreter60Version)
    val verifier = org.ergoplatform.wallet.interpreter.ErgoInterpreter(restrictedContext.currentParameters)
    val prefixState = restrictedState.withTransactions(Seq(txs.head))
    val restrictedCosts = txs.map { tx =>
      val validated = prefixState.validateWithCost(tx, restrictedContext, consensusBudget,
        Some(verifier), softFieldsAllowed = true)
      withClue("Each transaction must validate individually under the restricted consensus context: ") {
        validated.isSuccess shouldBe true
      }
      validated.get
    }
    restrictedCosts.foreach(_ should be <= consensusBudget)
    restrictedCosts.sum should be > consensusBudget
    val (input, ordering, invalid) = select(sources, Seq(txs.last), Header.Interpreter60Version,
      prefix = Seq(txs.head), maxCost = consensusBudget, protocolCost = consensusBudget)
    input shouldBe empty
    ordering shouldBe empty
    invalid shouldBe empty
  }

  property("an accepted prefix collects only the optional fee chunk that fits") {
    val sources = (0 to 100).map(i => box(i.toByte))
    val accepted = sources.map(payment)
    val testProtocolCost = 10000000
    val (_, rewards, _) = select(sources, Seq.empty, Header.Interpreter60Version,
      prefix = accepted, maxCost = testProtocolCost, protocolCost = testProtocolCost)
    rewards should have size 2
    val budget = sectionSize(accepted ++ rewards.take(1))
    sectionSize(accepted ++ rewards) should be > budget
    val (input, ordering, invalid) = select(sources, Seq.empty, Header.Interpreter60Version,
      prefix = accepted, maxCost = testProtocolCost, protocolCost = testProtocolCost, maxSize = budget)
    input shouldBe empty
    invalid shouldBe empty
    ordering.map(_.id) shouldBe rewards.take(1).map(_.id)
    val collectedIds = ordering.flatMap(_.inputs).map(in => bytesToId(in.boxId)).toSet
    assertFeeReward(ordering.head, accepted.filter(tx => collectedIds.contains(bytesToId(tx.outputs.last.id))))
    val unspent = org.ergoplatform.nodeView.state.ErgoState.newBoxes(accepted ++ ordering)
      .filter(b => b.propositionBytes.toSeq == feeTree.bytes.toSeq)
    unspent.map(b => bytesToId(b.id)).toSet shouldBe
      accepted.map(tx => bytesToId(tx.outputs.last.id)).toSet.diff(collectedIds)
    unspent should not be empty
  }

  property("a new ordering payment retains its required fee chunk ahead of optional prefix fees") {
    val prefixSources = (0 until 100).map(i => box(i.toByte))
    val newSource = box((-1).toByte, orderingTree)
    val sources = prefixSources :+ newSource
    val accepted = prefixSources.map(payment)
    val fresh = payment(newSource)
    val testProtocolCost = 10000000
    val (_, complete, _) = select(sources, Seq(fresh), Header.Interpreter60Version,
      prefix = accepted, maxCost = testProtocolCost, protocolCost = testProtocolCost)
    complete.head.id shouldBe fresh.id
    val rewards = complete.tail
    rewards should have size 2
    val (required, optional) = rewards.partition(_.inputs.exists(in =>
      bytesToId(in.boxId) == bytesToId(fresh.outputs.last.id)))
    required should have size 1
    optional should have size 1
    val budget = sectionSize(accepted ++ Seq(fresh) ++ required)
    sectionSize(accepted ++ complete) should be > budget
    val (input, ordering, invalid) = select(sources, Seq(fresh), Header.Interpreter60Version,
      prefix = accepted, maxCost = testProtocolCost, protocolCost = testProtocolCost, maxSize = budget)
    input shouldBe empty
    invalid shouldBe empty
    ordering.map(_.id) shouldBe (Seq(fresh) ++ required).map(_.id)
    val collectedIds = required.head.inputs.map(in => bytesToId(in.boxId)).toSet
    assertFeeReward(required.head, (accepted :+ fresh).filter(tx => collectedIds.contains(bytesToId(tx.outputs.last.id))))
    val unspent = org.ergoplatform.nodeView.state.ErgoState.newBoxes(accepted ++ ordering)
      .filter(b => b.propositionBytes.toSeq == feeTree.bytes.toSeq)
    unspent.map(b => bytesToId(b.id)).toSet shouldBe
      accepted.map(tx => bytesToId(tx.outputs.last.id)).toSet.diff(collectedIds)
    unspent should not be empty
  }

  property("fee reward batches consume all fee boxes when there are more than one hundred") {
    val batchSize = 100
    val sources = (0 to batchSize).map(i => box(i.toByte))
    val payments = sources.map(payment)
    val us = state(sources)
    val context = us.stateContext.upcoming(defaultMinerPk.value, 1L,
      settings.chainSettings.initialNBits, Array.emptyByteArray, emptyVSUpdate, Header.InitialVersion)
    val rewards = CandidateGenerator.collectRewards(None, us.stateContext.currentHeight,
      payments, defaultMinerPk, context)
    rewards should have size 2
    val consumed = rewards.flatMap(_.inputs).map(in => bytesToId(in.boxId))
    consumed should contain theSameElementsAs payments.map(tx => bytesToId(tx.outputs.last.id))
    consumed.distinct.size shouldBe payments.size
    rewards.foreach { reward =>
      reward.inputs should not be empty
      reward.inputs.size should be <= batchSize
      val consumedIds = reward.inputs.map(in => bytesToId(in.boxId)).toSet
      assertFeeReward(reward, payments.filter(tx => consumedIds.contains(bytesToId(tx.outputs.last.id))))
    }
    rewards.flatMap(_.outputs).map(_.value).sum shouldBe payments.size.toLong * fee
    us.proofsForTransactions(payments ++ rewards).isSuccess shouldBe true
  }
}
