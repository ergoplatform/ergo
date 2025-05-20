package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox.{R4, TokenId}
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings._
import org.ergoplatform.utils.{ErgoCompilerHelpers, ErgoCorePropertyTest, ErgoStateContextHelpers}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input}
import scorex.util.{ModifierId, bytesToId}
import sigmastate.eval.Extensions._
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.sdk.wallet.protocol.context.TransactionContext
import org.ergoplatform.settings.Parameters.MaxBlockCostIncrease
import org.ergoplatform.settings.ValidationRules.{bsBlockTransactionsCost, txAssetsInOneBox}
import org.ergoplatform.wallet.boxes.{ErgoBoxAssetExtractor, ErgoBoxSerializer}
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import org.ergoplatform.wallet.protocol.context.InputContext
import org.scalacheck.Gen
import sigma.util.BenchmarkUtil
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigma.{Colls, VersionContext}
import sigma.ast.ErgoTree.DefaultHeader
import sigma.ast.{AND, ErgoTree, TrueLeaf, UnsignedBigIntConstant}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.ErgoTreeSerializer
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.helpers.TestingHelpers._

import java.math.BigInteger
import scala.util.{Random, Try}

class ErgoNodeTransactionSpec extends ErgoCorePropertyTest with ErgoCompilerHelpers with ErgoStateContextHelpers {

  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  private val emptyModifierId: ModifierId = bytesToId(Array.fill(32)(0.toByte))


  private def checkTx(from: IndexedSeq[ErgoBox], tx: ErgoTransaction): Try[Int] = {
    tx.statelessValidity().flatMap(_ => tx.statefulValidity(from, emptyDataBoxes, emptyStateContext))
  }

  private def modifyValue(boxCandidate: ErgoBoxCandidate, delta: Long): ErgoBoxCandidate = {
    new ErgoBoxCandidate(
      boxCandidate.value + delta,
      boxCandidate.ergoTree,
      boxCandidate.creationHeight,
      boxCandidate.additionalTokens,
      boxCandidate.additionalRegisters)
  }

  private def modifyAsset(boxCandidate: ErgoBoxCandidate,
                          deltaFn: Long => Long,
                          idToskip: TokenId): ErgoBoxCandidate = {
    val assetId = boxCandidate.additionalTokens.find(t => t._1 != idToskip).get._1

    val tokens = boxCandidate.additionalTokens.map { case (id, amount) =>
      if (id == assetId) assetId -> deltaFn(amount) else assetId -> amount
    }

    new ErgoBoxCandidate(
      boxCandidate.value,
      boxCandidate.ergoTree,
      boxCandidate.creationHeight,
      tokens,
      boxCandidate.additionalRegisters)
  }

  private def updateAnAsset(tx: ErgoTransaction, from: IndexedSeq[ErgoBox], deltaFn: Long => Long) = {
    val updCandidates = tx.outputCandidates.foldLeft(IndexedSeq[ErgoBoxCandidate]() -> false) { case ((seq, modified), ebc) =>
      if (modified) {
        (seq :+ ebc) -> true
      } else {
        if (ebc.additionalTokens.nonEmpty && ebc.additionalTokens.exists(t => !java.util.Arrays.equals(t._1.toArray, from.head.id))) {
          (seq :+ modifyAsset(ebc, deltaFn, from.head.id.toTokenId)) -> true
        } else {
          (seq :+ ebc) -> false
        }
      }
    }._1
    tx.copy(outputCandidates = updCandidates)
  }

  property("monotonic creation height") {

    def updateOutputHeight(box: ErgoBoxCandidate, value: Int): ErgoBoxCandidate = {
      new ErgoBoxCandidate(box.value, box.ergoTree, value, box.additionalTokens, box.additionalRegisters)
    }

    def updateInputHeight(box: ErgoBox): ErgoBox = {
      val creationHeight = scala.util.Random.nextInt(1000) + 1
      new ErgoBox(box.value, box.ergoTree, box.additionalTokens, box.additionalRegisters,
        box.transactionId, box.index, creationHeight)
    }

    // retuns random transaction along with inputs,
    // and a boolean flag, if the latter is true, monotonic creation height rule holds,
    // otherwise, it does not hold
    val txGen = boxesGenTemplate(minAssets = 0, maxAssets = 5, minInputs = 5, maxInputs = 10, propositionGen = trueLeafGen).map { case (boxes, _) =>
      boxes.map(updateInputHeight)
    }.map { boxes =>
      val fixed = Random.nextBoolean()
      val maxHeight = boxes.map(_.creationHeight).max
      val preUnsignedTx = validUnsignedTransactionFromBoxes(boxes)
      val unsignedTx = if (fixed) {
        preUnsignedTx.copy(outputCandidates = preUnsignedTx.outputCandidates.map { out =>
          val creationHeight = maxHeight + Random.nextInt(3)
          updateOutputHeight(out, creationHeight)
        })
      } else {
        preUnsignedTx.copy(outputCandidates = preUnsignedTx.outputCandidates.map { out =>
          val creationHeight = maxHeight - (Random.nextInt(maxHeight) + 1)
          updateOutputHeight(out, creationHeight)
        })
      }
      val tx = defaultProver.sign(unsignedTx, boxes, IndexedSeq.empty, emptyStateContext)
        .map(ErgoTransaction.apply)
        .get
      (boxes, tx, fixed)
    }

    forAll(txGen) { case (boxes, tx, fixed) =>
      // with initial block version == 1, monotonic rule does not work
      tx.statefulValidity(boxes, IndexedSeq.empty, stateContextForTx(tx, blockVersion = 1, settings)).isSuccess shouldBe true

      // with pre-5.0 block version == 2, monotonic rule does not work as well
      tx.statefulValidity(boxes, IndexedSeq.empty, stateContextForTx(tx, blockVersion = 2, settings)).isSuccess shouldBe true

      // starting from block version == 3, monotonic rule works,
      // so validation fails if transaction is not following the rule (fixed == false)
      val ctx3 = stateContextForTx(tx, blockVersion = 3, settings)
      if (fixed) {
        tx.statefulValidity(boxes, IndexedSeq.empty, ctx3).isSuccess shouldBe true
      } else {
        val txFailure = tx.statefulValidity(boxes, IndexedSeq.empty, ctx3)
        txFailure.isSuccess shouldBe false
        val cause = txFailure.toEither.left.get.getMessage
        val expectedMessage = ValidationRules.errorMessage(ValidationRules.txMonotonicHeight, "", emptyModifierId, ErgoTransaction.modifierTypeId)
        cause should startWith(expectedMessage)
      }
    }
  }

  property("a valid transaction is valid") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      tx.statelessValidity().isSuccess shouldBe true
      tx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe true
    }
  }

  property("ergo preservation law holds") {
    forAll(validErgoTransactionGen, smallPositiveInt) { case ((from, tx), deltaAbs) =>
      val delta = if (Random.nextBoolean()) -deltaAbs else deltaAbs

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, delta) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity().isSuccess &&
        wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to create a negative-value output") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val negValue = Math.min(Math.abs(Random.nextLong()), Long.MaxValue - tx.outputCandidates.head.value)
      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, -(tx.outputCandidates.head.value + negValue)) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity().isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to overflow ergo tokens") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val overflowSurplus = (Long.MaxValue - tx.outputCandidates.map(_.value).sum) + 1

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, overflowSurplus) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity().isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("assets preservation law holds") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      checkTx(from, updateAnAsset(tx, from, _ + 1)) shouldBe 'failure
    }
  }

  property("impossible to create an asset of negative amount") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      checkTx(from, updateAnAsset(tx, from, _ => -1)) shouldBe 'failure
    }
  }

  property("impossible to create an asset of zero amount") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      checkTx(from, updateAnAsset(tx, from, _ => 0)) shouldBe 'failure
    }
  }

  property("impossible to overflow an asset value") {
    val gen = validErgoTransactionGenTemplate(minAssets = 1, maxAssets = 1, maxInputs = 16, propositionGen = trueLeafGen)
    forAll(gen) { case (from, tx) =>
      val tokenOpt = tx.outputCandidates.flatMap(_.additionalTokens.toArray)
        .groupBy(_._1).find(_._2.size >= 2)

      whenever(tokenOpt.nonEmpty) {
        val tokenId = tokenOpt.get._1
        val tokenAmount = tokenOpt.get._2.map(_._2).sum

        var modified = false
        val updCandidates = tx.outputCandidates.map { c =>
          val updTokens = c.additionalTokens.map { case (id, amount) =>
            if (!modified && id == tokenId) {
              modified = true
              id -> ((Long.MaxValue - tokenAmount) + amount + 1)
            } else {
              id -> amount
            }
          }
          new ErgoBoxCandidate(c.value, c.ergoTree, startHeight, updTokens, c.additionalRegisters)
        }

        val wrongTx = tx.copy(outputCandidates = updCandidates)
        checkTx(from, wrongTx) shouldBe 'failure
      }
    }
  }

  property("stateful validation should catch false proposition") {
    val propositionGen = Gen.const(Constants.FalseTree)
    val gen = validErgoTransactionGenTemplate(1, 1, 1, propositionGen)
    forAll(gen) { case (from, tx) =>
      tx.statelessValidity().isSuccess shouldBe true
      val validity = tx.statefulValidity(from, emptyDataBoxes, emptyStateContext)
      validity.isSuccess shouldBe false
      val e = validity.failed.get
      e.getMessage should startWith(ValidationRules.errorMessage(ValidationRules.txScriptValidation, "", emptyModifierId, ErgoTransaction.modifierTypeId))
    }
  }

  property("assets usage correctly affects transaction total cost") {
    val txGen = validErgoTransactionGenTemplate(1, 1, 16, propositionGen = trueLeafGen)
    forAll(txGen) { case (from, tx) =>
      val initTxCost = tx.statefulValidity(from, emptyDataBoxes, emptyStateContext).get

      // already existing token from one of the inputs
      val existingToken = from.flatMap(_.additionalTokens.toArray).toSet.head
      // completely new token
      val randomToken = (scorex.util.Random.randomBytes().toTokenId, Random.nextInt(100000000).toLong)

      val in0 = from.last
      // new token added to the last input
      val modifiedIn0 = testBox(in0.value, in0.ergoTree, in0.creationHeight,
        in0.additionalTokens.toArray.toSeq :+ randomToken, in0.additionalRegisters, in0.transactionId, in0.index)
      val txInMod0 = tx.inputs.last.copy(boxId = modifiedIn0.id)

      val in1 = from.last
      // existing token added to the last input
      val modifiedIn1 = testBox(in1.value, in1.ergoTree, in1.creationHeight,
        in1.additionalTokens.toArray.toSeq :+ existingToken, in1.additionalRegisters, in1.transactionId, in1.index)
      val txInMod1 = tx.inputs.last.copy(boxId = modifiedIn1.id)

      val out0 = tx.outputs.last
      // new token added to the last output
      val modifiedOut0 = testBox(out0.value, out0.ergoTree, out0.creationHeight,
        out0.additionalTokens.toArray.toSeq :+ randomToken, out0.additionalRegisters, out0.transactionId, out0.index)
      // existing token added to the last output
      val modifiedOut1 = testBox(out0.value, out0.ergoTree, out0.creationHeight,
        out0.additionalTokens.toArray.toSeq :+ existingToken, out0.additionalRegisters, out0.transactionId, out0.index)

      // update transaction inputs and outputs accordingly
      val txMod0 = tx.copy(inputs = tx.inputs.init :+ txInMod0) // new token group added to one input
      val txMod1 = tx.copy(inputs = tx.inputs.init :+ txInMod1) // existing token added to one input
      val txMod2 = tx.copy(inputs = tx.inputs.init :+ txInMod0, // new token group added to one input and one output
        outputCandidates = tx.outputCandidates.init :+ modifiedOut0)
      val txMod3 = tx.copy(inputs = tx.inputs.init :+ txInMod1, // existing token added to one input and one output
        outputCandidates = tx.outputCandidates.init :+ modifiedOut1)

      val inputIncTxCost0 = txMod0.statefulValidity(from.init :+ modifiedIn0, emptyDataBoxes, emptyStateContext).get
      val inputIncTxCost1 = txMod1.statefulValidity(from.init :+ modifiedIn1, emptyDataBoxes, emptyStateContext).get
      val outputIncTxCost0 = txMod2.statefulValidity(from.init :+ modifiedIn0, emptyDataBoxes, emptyStateContext).get
      val outputIncTxCost1 = txMod3.statefulValidity(from.init :+ modifiedIn1, emptyDataBoxes, emptyStateContext).get

      (inputIncTxCost0 - initTxCost) shouldEqual Parameters.TokenAccessCostDefault * 2 // one more group + one more token in total
      (inputIncTxCost1 - initTxCost) shouldEqual Parameters.TokenAccessCostDefault // one more token in total
      (outputIncTxCost0 - inputIncTxCost0) shouldEqual Parameters.TokenAccessCostDefault * 2
      (outputIncTxCost1 - inputIncTxCost1) shouldEqual Parameters.TokenAccessCostDefault
    }
  }

  property("spam simulation (transaction validation cost with too many tokens exceeds block limit)") {
    val bxsQty = 392 // with greater value test is failing with collection size exception
    val (inputs, tx) = validErgoTransactionGenTemplate(1, 1, 16).sample.get // it takes too long to test with `forAll`
    val tokens = (0 until 255).map(_ => (scorex.util.Random.randomBytes().toTokenId, Random.nextLong))
    val (in, out) = {
      val in0 = inputs.head
      val out0 = tx.outputs.head
      val inputsMod = (0 until bxsQty).map { i =>
        testBox(10000000000L, in0.ergoTree, in0.creationHeight,
          tokens, in0.additionalRegisters, in0.transactionId, i.toShort)
      }
      val outputsMod = (0 until bxsQty).map { i =>
        testBox(10000000000L, out0.ergoTree, out0.creationHeight,
          tokens, out0.additionalRegisters, out0.transactionId, i.toShort)
      }
      inputsMod -> outputsMod
    }
    val inputsPointers = {
      val inSample = tx.inputs.head
      (0 until bxsQty).map(i => inSample.copy(boxId = in(i).id))
    }
    val txMod = tx.copy(inputs = inputsPointers, outputCandidates = out)
    val validFailure = txMod.statefulValidity(in, emptyDataBoxes, emptyStateContext)
    validFailure.failed.get.getMessage should startWith(ValidationRules.errorMessage(txAssetsInOneBox, "", emptyModifierId, ErgoTransaction.modifierTypeId).take(30))
  }

  property("transaction with too many inputs should be rejected") {

    //we assume that verifier must finish verification of any script in less time than 250K hash calculations
    // (for the Blake2b256 hash function over a single block input)
    val Timeout: Long = {
      val hf = Blake2b256

      //just in case to heat up JVM
      (1 to 5000000).foreach(i => hf(s"$i-$i"))

      val t0 = System.currentTimeMillis()
      (1 to 250000).foreach(i => hf(s"$i"))
      val t = System.currentTimeMillis()
      t - t0
    }

    val gen = validErgoTransactionGenTemplate(0, 0, 2000, trueLeafGen)
    val (from, tx) = gen.sample.get
    tx.statelessValidity().isSuccess shouldBe true

    //check that spam transaction is being rejected quickly
    implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)
    val (validity, time0) = BenchmarkUtil.measureTime(tx.statefulValidity(from, IndexedSeq(), emptyStateContext))
    validity.isSuccess shouldBe false
    assert(time0 <= Timeout)

    val cause = validity.failed.get.getMessage
    cause should startWith(ValidationRules.errorMessage(bsBlockTransactionsCost, "", emptyModifierId, ErgoTransaction.modifierTypeId).take(30))

    //check that spam transaction validation with no cost limit is indeed taking too much time
    import Parameters._
    val maxCost = (Int.MaxValue - 10) / 10 // cannot use Int.MaxValue directly due to overflow when it is converted to block cost
    val ps = Parameters(0, DefaultParameters.updated(MaxBlockCostIncrease, maxCost), emptyVSUpdate)
    val sc = new ErgoStateContext(Seq.empty, None, genesisStateDigest, ps, ErgoValidationSettings.initial,
      VotingData.empty)(settings.chainSettings)
      .upcoming(org.ergoplatform.mining.group.generator,
        0L,
        settings.chainSettings.initialNBits,
        Array.fill(3)(0.toByte),
        ErgoValidationSettingsUpdate.empty,
        0.toByte)
    val (_, time) = BenchmarkUtil.measureTime(
      tx.statefulValidity(from, IndexedSeq(), sc)(verifier)
    )

    assert(time > Timeout)
  }

  property("transaction cost") {
    def paramsWith(manualCost: Int) = Parameters(
      0,
      Parameters.DefaultParameters + (MaxBlockCostIncrease -> manualCost),
      ErgoValidationSettingsUpdate.empty
    )

    val gen = validErgoTransactionGenTemplate(0, 0, 10, trueLeafGen)
    val (from, tx) = gen.sample.get
    tx.statelessValidity().isSuccess shouldBe true

    // calculate costs manually
    val initialCost: Long =
      tx.inputs.size * parameters.inputCost +
        tx.dataInputs.size * parameters.dataInputCost +
        tx.outputs.size * parameters.outputCost +
        ErgoInterpreter.interpreterInitCost
    val (outAssets, outAssetsNum) = tx.outAssetsTry.get
    val (inAssets, inAssetsNum) = ErgoBoxAssetExtractor.extractAssets(from).get
    val totalAssetsAccessCost =
      (outAssetsNum + inAssetsNum) * parameters.tokenAccessCost +
        (inAssets.size + outAssets.size) * parameters.tokenAccessCost
    val scriptsValidationCosts = tx.inputs.size + 1 // +1 for the block to JIT cost scaling
    val approxCost: Int = (initialCost + totalAssetsAccessCost + scriptsValidationCosts).toInt


    // check that validation pass if cost limit equals to approximated cost
    val sc = stateContextWith(paramsWith(approxCost))
    sc.currentParameters.maxBlockCost shouldBe approxCost
    val calculatedCost = tx.statefulValidity(from, IndexedSeq(), sc)(ErgoInterpreter(sc.currentParameters)).get
    approxCost - calculatedCost <= 1 shouldBe true

    // transaction exceeds computations limit
    val sc2 = stateContextWith(paramsWith(approxCost - 1))
    tx.statefulValidity(from, IndexedSeq(), sc2)(ErgoInterpreter(sc2.currentParameters)) shouldBe 'failure

    // transaction exceeds computations limit due to non-zero accumulatedCost
    tx.statefulValidity(from, IndexedSeq(), sc, accumulatedCost = 1)(ErgoInterpreter(sc.currentParameters)) shouldBe 'failure
  }

  property("cost accumulated correctly across inputs") {
    val accInitCost = 100000

    def inputCost(tx: ErgoTransaction, from: IndexedSeq[ErgoBox]): Long = {
      val idx = 0
      val input = tx.inputs(idx)
      val proof = input.spendingProof
      val transactionContext = TransactionContext(from, IndexedSeq(), tx)
      val inputContext = InputContext(idx.toShort, proof.extension)

      val ctx = new ErgoContext(
        emptyStateContext, transactionContext, inputContext,
        costLimit = emptyStateContext.currentParameters.maxBlockCost,
        initCost = 0)

      val messageToSign = tx.messageToSign

      val inputCost = verifier.verify(from(idx).ergoTree, ctx, proof, messageToSign).get._2

      inputCost
    }

    forAll(smallPositiveInt) { inputsNum =>

      val nonTrivialTrueGen = Gen.const(ErgoTree.withSegregation(DefaultHeader, AND(Seq(TrueLeaf, TrueLeaf)).toSigmaProp))
      val gen = validErgoTransactionGenTemplate(0, 0, inputsNum, nonTrivialTrueGen)
      val (from, tx) = gen.sample.get
      tx.statelessValidity().isSuccess shouldBe true

      tx.inputs.length shouldBe inputsNum

      val tokenAccessCost = emptyStateContext.currentParameters.tokenAccessCost

      val txCost = tx.statefulValidity(from, IndexedSeq(), emptyStateContext, accInitCost).get

      val (inAssets, inAssetsNum): (Map[Seq[Byte], Long], Int) = ErgoBoxAssetExtractor.extractAssets(from).get
      val (outAssets, outAssetsNum): (Map[Seq[Byte], Long], Int) = ErgoBoxAssetExtractor.extractAssets(tx.outputs).get

      val assetsCost = inAssetsNum * tokenAccessCost + inAssets.size * tokenAccessCost +
        outAssetsNum * tokenAccessCost + outAssets.size * tokenAccessCost

      val unsignedTx = UnsignedErgoTransaction(tx.inputs, tx.dataInputs, tx.outputCandidates)
      val signerTxCost =
        defaultProver.signInputs(unsignedTx, from, Vector.empty, emptyStateContext, TransactionHintsBag.empty).get._2

      val signerTxCostWithInitCost = signerTxCost + accInitCost
      signerTxCostWithInitCost shouldBe txCost // signer and verifier costs should be the same

      val initialCost: Long =
        tx.inputs.size * parameters.inputCost +
          tx.dataInputs.size * parameters.dataInputCost +
          tx.outputs.size * parameters.outputCost +
          ErgoInterpreter.interpreterInitCost +
          assetsCost

      txCost shouldBe (accInitCost + initialCost + inputCost(tx, from) * inputsNum)
    }
  }

  private val v60scripts = Array(
    "sigmaProp(Global.serialize(2).size > 0)",
    """{
      | val b = 1.toByte
      | b.toBits == Coll(false, false, false, false, false, false, false, true)
      |}""".stripMargin,
    """{
      |  val b = bigInt("-1")
      |  val m = unsignedBigInt("5")
      |  val ub = b.toUnsignedMod(m)
      |  ub >= 0
      | } """.stripMargin,
    """{
      |   val bs1 = fromBase16("014201ad7fffba807080ce7f808001b4771880bdffbd7fff857f8db17fdc89b0015bff0189486d80934a84015e03c064c6d100c17fae55bf01f380007fbb00ff01d9000000013f0680d10015ff11c4b7ffff01ffb2d27f0001050064010080ffb93e7fff4cc15a2bffdd770480007f8080ff8026802f7f51007f004b01ff0080002380a581e8c0ca81dfca62007fa7ff01860019f37f3c4491067cd1001ee07e570fe8689b80ff11017aff7408703927a2bbc4fa0200000000032bdb4c610ddf0078b4f31f5a36bff7b003704dfc93b201fedd8fe331d9ff1102008d7fff010d80ff")
      |   val oh1 = Global.deserializeTo[Option[Header]](bs1)
      |   val bs2 = fromBase16("00")
      |   val oh2 = Global.deserializeTo[Option[Header]](bs2)
      |   sigmaProp(oh1.isDefined && oh2.isEmpty)
      | } """.stripMargin
  )

  property("Execution of 6.0 Ergoscript - v3 tree") {

    v60scripts.foreach { script =>
      val protocolVersion = 4.toByte
      val treeVersion = (protocolVersion - 1).toByte
      val params = new Parameters(0, DevnetLaunchParameters.parametersTable.updated(123, protocolVersion), ErgoValidationSettingsUpdate.empty)

      val stateContext = emptyStateContext.copy(currentParameters = params)(chainSettings)
      stateContext.blockVersion shouldBe protocolVersion

      val ergoTree = compileSourceV6(script, treeVersion)

      ergoTree.root.isRight shouldBe true // parsed
      ergoTree.version shouldBe treeVersion

      val b = new ErgoBox(1000000000L, ergoTree, Colls.emptyColl,
          Map.empty, ModifierId @@ "c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742",
          0, 0)

      val input = Input(b.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))

      val oc = new ErgoBoxCandidate(b.value, b.ergoTree, b.creationHeight)

      val utx = new ErgoTransaction(IndexedSeq(input), IndexedSeq.empty, IndexedSeq(oc))

      val f = utx.statefulValidity(IndexedSeq(b), IndexedSeq.empty, stateContext, 0)(defaultProver)
      f.isSuccess shouldBe true
    }
  }

  property("Execution of 6.0 Ergoscript reducing to false") {

    val protocolVersion = 4.toByte
    val treeVersion = (protocolVersion - 1).toByte
    val params = new Parameters(0, DevnetLaunchParameters.parametersTable.updated(123, protocolVersion), ErgoValidationSettingsUpdate.empty)

    val stateContext = emptyStateContext.copy(currentParameters = params)(chainSettings)
    stateContext.blockVersion shouldBe protocolVersion

    val ergoTree = compileSourceV6("sigmaProp(Global.serialize(2).size <= 0)", treeVersion)

    ergoTree.root.isRight shouldBe true // parsed

    val b = new ErgoBox(1000000000L, ergoTree, Colls.emptyColl,
      Map.empty, ModifierId @@ "c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742",
      0, 0)
    val input = Input(b.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))

    val oc = new ErgoBoxCandidate(b.value, b.ergoTree, b.creationHeight)

    val utx = new ErgoTransaction(IndexedSeq(input), IndexedSeq.empty, IndexedSeq(oc))

    val f = utx.statefulValidity(IndexedSeq(b), IndexedSeq.empty, stateContext, 0)(defaultProver)
    f.isSuccess shouldBe false
    f.failed.get.getMessage.contains("#0 => Success((false,") shouldBe true
  }

  property("6.0 execution of unparsed Ergoscript reducing to false") {

    val protocolVersion = 4.toByte
    val params = new Parameters(0, DevnetLaunchParameters.parametersTable.updated(123, protocolVersion), ErgoValidationSettingsUpdate.empty)

    val stateContext = emptyStateContext.copy(currentParameters = params)(chainSettings)
    stateContext.blockVersion shouldBe protocolVersion

    // v2 tree but v3 instructions
    val ergoTree = DefaultSerializer.deserializeErgoTree(Base16.decode("1a150206022edf0580fcf622d193db060873007e730106").get)
    ergoTree.root.isRight shouldBe false

    val b = new ErgoBox(1000000000L, ergoTree, Colls.emptyColl,
      Map.empty, ModifierId @@ "c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742",
      0, 0)
    val input = Input(b.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))

    val oc = new ErgoBoxCandidate(b.value, b.ergoTree, b.creationHeight)

    val utx = new ErgoTransaction(IndexedSeq(input), IndexedSeq.empty, IndexedSeq(oc))

    val f = utx.statefulValidity(IndexedSeq(b), IndexedSeq.empty, stateContext, 0)(defaultProver)
    f.isSuccess shouldBe false
  }

  property("Box can't contain 6.0 data types in registers") {
    val ubic = UnsignedBigIntConstant(new BigInteger("2"))
    val b = new ErgoBox(1000000000L, ErgoTreePredef.TrueProp(ErgoTree.defaultHeaderWithVersion(3.toByte)), Colls.emptyColl,
      Map(R4 -> ubic), ModifierId @@ "c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742",
      0, 0)

    VersionContext.withVersions(3, 3) {
      val bytes = ErgoBoxSerializer.toBytes(b)
      ErgoBoxSerializer.parseBytesTry(bytes).isSuccess shouldBe false
    }
  }

  property("Execution of v7 tree") {
    val tree2 = compileSource("sigmaProp(true)", 7, 7)
    val bs = Base16.encode(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree2))
    bs shouldBe "1f06010101d17300"
    println(bs)
    println(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(bs).get))

  }

}
