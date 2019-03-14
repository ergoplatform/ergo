package org.ergoplatform.modifiers.mempool

import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.settings.{Constants, LaunchParameters, Parameters}
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import org.scalacheck.Gen
import scalan.util.BenchmarkUtil
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base16

import scala.util.Random

class ErgoTransactionSpec extends ErgoPropertyTest {

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
    val assetId = boxCandidate.additionalTokens.find(t => !java.util.Arrays.equals(t._1, idToskip)).get._1

    val tokens = boxCandidate.additionalTokens.map { case (id, amount) =>
      if (java.util.Arrays.equals(id, assetId)) assetId -> deltaFn(amount) else assetId -> amount
    }

    new ErgoBoxCandidate(
      boxCandidate.value,
      boxCandidate.ergoTree,
      boxCandidate.creationHeight,
      tokens,
      boxCandidate.additionalRegisters)
  }

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)

  property("serialization vector") {
    // test vector, that specifies transaction json and bytes representation.
    // ensures that bytes transaction representation was not changed
    val bytes = Base16.decode("02e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6400002e9798d7eb0cd867f6dc29872f80de64c04cef10a99a58d007ef7855f0acbdb9000001f97d1dc4626de22db836270fe1aa004b99970791e4557de8f486f6d433b81195026df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6403da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100fa979af8988ce7010008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000000").get
    val tx = ErgoTransactionSerializer.parseBytes(bytes)
    val str = "{\"id\":\"663ae91ab7145a4f42b5509e1a2fb0469b7cb46ea87fdfd90e0b4c8ef29c2493\",\"inputs\":[{\"boxId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"spendingProof\":{\"proofBytes\":\"\",\"extension\":{}}},{\"boxId\":\"2e9798d7eb0cd867f6dc29872f80de64c04cef10a99a58d007ef7855f0acbdb9\",\"spendingProof\":{\"proofBytes\":\"\",\"extension\":{}}}],\"dataInputs\":[{\"boxId\":\"f97d1dc4626de22db836270fe1aa004b99970791e4557de8f486f6d433b81195\"}],\"outputs\":[{\"boxId\":\"69e05b68715caaa4ca58ba59a8c8c7e031d42ad890b05f87021a28617c1e70d5\",\"value\":524940416256346,\"proposition\":\"100108cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a21767300\",\"assets\":[{\"tokenId\":\"6df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963\",\"amount\":226153050},{\"tokenId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"amount\":536110126}],\"creationHeight\":0,\"additionalRegisters\":{}},{\"boxId\":\"556a9a3ec7880d468e56d44e75898cf8a32f6a07344895fa6b5cf34edf101a59\",\"value\":524940416256346,\"proposition\":\"100108cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a21767300\",\"assets\":[{\"tokenId\":\"6df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963\",\"amount\":226153050},{\"tokenId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"amount\":536110126}],\"creationHeight\":0,\"additionalRegisters\":{}},{\"boxId\":\"16385b5b83992629909c7e004ed0421229ed3587162ce6f29b2df129472e3909\",\"value\":1016367755463674,\"proposition\":\"100108cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a21767300\",\"assets\":[],\"creationHeight\":0,\"additionalRegisters\":{}}],\"size\":329}"
    tx.asJson.noSpaces shouldBe str
  }

  property("a valid transaction is valid") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe true
    }
  }

  property("ergo preservation law holds") {
    forAll(validErgoTransactionGen, smallPositiveInt) { case ((from, tx), deltaAbs) =>
      val delta = if (Random.nextBoolean()) -deltaAbs else deltaAbs

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, delta) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess &&
        wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to create a negative-value output") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val negValue = Math.min(Math.abs(Random.nextLong()), Long.MaxValue - tx.outputCandidates.head.value)
      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, -(tx.outputCandidates.head.value + negValue)) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to overflow ergo tokens") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val overflowSurplus = (Long.MaxValue - tx.outputCandidates.map(_.value).sum) + 1

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, overflowSurplus) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  private def updateAnAsset(tx: ErgoTransaction, from: IndexedSeq[ErgoBox], deltaFn: Long => Long) = {
    val updCandidates = tx.outputCandidates.foldLeft(IndexedSeq[ErgoBoxCandidate]() -> false) { case ((seq, modified), ebc) =>
      if (modified) {
        (seq :+ ebc) -> true
      } else {
        if (ebc.additionalTokens.nonEmpty && ebc.additionalTokens.exists(t => !java.util.Arrays.equals(t._1, from.head.id))) {
          (seq :+ modifyAsset(ebc, deltaFn, Digest32 @@ from.head.id)) -> true
        } else {
          (seq :+ ebc) -> false
        }
      }
    }._1
    tx.copy(outputCandidates = updCandidates)
  }

  property("assets preservation law holds") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      val wrongTx = updateAnAsset(tx, from, _ + 1)
      wrongTx.statelessValidity.isSuccess shouldBe true
      wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to create an asset of non-positive amount") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      val wrongTx = updateAnAsset(tx, from, _ => -1)
      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to overflow an asset value") {
    val gen = validErgoTransactionGenTemplate(1, 1, 8, 16)
    forAll(gen) { case (from, tx) =>
      val tokenOpt = tx.outputCandidates.flatMap(_.additionalTokens).map(t => ByteArrayWrapper.apply(t._1) -> t._2)
        .groupBy(_._1).find(_._2.size >= 2)

      whenever(tokenOpt.nonEmpty) {
        val tokenId = tokenOpt.get._1
        val tokenAmount = tokenOpt.get._2.map(_._2).sum

        var modified = false
        val updCandidates = tx.outputCandidates.map { c =>
          val updTokens = c.additionalTokens.map { case (id, amount) =>
            if (!modified && ByteArrayWrapper(id) == tokenId) {
              modified = true
              id -> ((Long.MaxValue - tokenAmount) + amount + 1)
            } else {
              id -> amount
            }
          }
          new ErgoBoxCandidate(c.value, c.ergoTree, startHeight, updTokens, c.additionalRegisters)
        }

        val wrongTx = tx.copy(outputCandidates = updCandidates)
        wrongTx.statelessValidity.isSuccess shouldBe false
        wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
      }
    }
  }

  property("stateful validation should catch false proposition") {
    val propositionGen = Gen.const(Constants.FalseLeaf)
    val gen = validErgoTransactionGenTemplate(1, 1, 1, 1, propositionGen)
    forAll(gen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      val validity = tx.statefulValidity(from, emptyDataBoxes, emptyStateContext)
      validity.isSuccess shouldBe false
      val e = validity.failed.get
      log.info(s"Validation message: ${e.getMessage}", e)
      e.getMessage should startWith("Input script verification failed for input #0")
    }
  }

  property("assets usage correctly affects transaction total cost") {
    val txGen = validErgoTransactionGenTemplate(1, 1, 8, 16)
    forAll(txGen) { case (from, tx) =>
      val initTxCost = tx.statefulValidity(from, emptyDataBoxes, emptyStateContext).get

      // already existing token from one of the inputs
      val existingToken = from.flatMap(_.additionalTokens).toSet.head
      // completely new token
      val randomToken = (Digest32 @@ scorex.util.Random.randomBytes(), Random.nextInt(100000000).toLong)

      val in0 = from.last
      // new token added to the last input
      val modifiedIn0 = ErgoBox(in0.value, in0.ergoTree, in0.creationHeight,
        in0.additionalTokens :+ randomToken, in0.additionalRegisters, in0.transactionId, in0.index)
      val txInMod0 = tx.inputs.last.copy(boxId = modifiedIn0.id)

      val in1 = from.last
      // existing token added to the last input
      val modifiedIn1 = ErgoBox(in1.value, in1.ergoTree, in1.creationHeight,
        in1.additionalTokens :+ existingToken, in1.additionalRegisters, in1.transactionId, in1.index)
      val txInMod1 = tx.inputs.last.copy(boxId = modifiedIn1.id)

      val out0 = tx.outputs.last
      // new token added to the last output
      val modifiedOut0 = ErgoBox(out0.value, out0.ergoTree, out0.creationHeight,
        out0.additionalTokens :+ randomToken, out0.additionalRegisters, out0.transactionId, out0.index)
      // existing token added to the last output
      val modifiedOut1 = ErgoBox(out0.value, out0.ergoTree, out0.creationHeight,
        out0.additionalTokens :+ existingToken, out0.additionalRegisters, out0.transactionId, out0.index)

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
    val bxsQty = 400
    val (inputs, tx) = validErgoTransactionGenTemplate(1, 1, 8, 16).sample.get // it takes too long to test with `forAll`
    val tokens = (0 until 255).map(_ => (Digest32 @@ scorex.util.Random.randomBytes(), Random.nextInt(100000000).toLong))
    val (in, out) = {
      val in0 = inputs.head
      val out0 = tx.outputs.head
      val inputsMod = (0 until bxsQty).map { i =>
        ErgoBox(10000000000L, in0.ergoTree, in0.creationHeight,
          tokens, in0.additionalRegisters, in0.transactionId, i.toShort)
      }
      val outputsMod = (0 until bxsQty).map { i =>
        ErgoBox(10000000000L, out0.ergoTree, out0.creationHeight,
          tokens, out0.additionalRegisters, out0.transactionId, i.toShort)
      }
      inputsMod -> outputsMod
    }
    val inputsPointers = {
      val inSample = tx.inputs.head
      (0 until bxsQty).map(i => inSample.copy(boxId = in(i).id))
    }
    val txMod = tx.copy(inputs = inputsPointers, outputCandidates = out)
    val cost = txMod.statefulValidity(in, emptyDataBoxes, emptyStateContext).get
    cost shouldBe >(LaunchParameters.maxBlockCost)
  }

  property("transaction with too many inputs should be rejected") {

    //we assume that verifier must finish verification of any script in less time than 3M hash calculations
    // (for the Blake2b256 hash function over a single block input)
    val Timeout: Long = {
      val block = Array.fill(16)(0: Byte)
      val hf = Blake2b256

      //just in case to heat up JVM
      (1 to 5000000).foreach(_ => hf(block))

      val t0 = System.currentTimeMillis()
      (1 to 3000000).foreach(_ => hf(block))
      val t = System.currentTimeMillis()
      t - t0
    }

    val gen = validErgoTransactionGenTemplate(0, 0, 2000, 5000, trueLeafGen)

    val (from, tx) = gen.sample.get

    tx.statelessValidity.isSuccess shouldBe true
    val (validity, time0) = BenchmarkUtil.measureTime(tx.statefulValidity(from, IndexedSeq(), emptyStateContext)(ErgoInterpreter(LaunchParameters)))
    validity.isSuccess shouldBe false
    assert(time0 <= Timeout)

    val cause = validity.failed.get.getCause
    Option(cause) shouldBe defined
    cause.getMessage should startWith("Estimated expression complexity")

    val verifier = ErgoInterpreter(Parameters(0, LaunchParameters.parametersTable.updated(Parameters.MaxBlockCostIncrease, Int.MaxValue)))
    val (_, time) = BenchmarkUtil.measureTime(tx.statefulValidity(from, IndexedSeq(), emptyStateContext)(verifier))

    assert(time > Timeout)
  }

}
