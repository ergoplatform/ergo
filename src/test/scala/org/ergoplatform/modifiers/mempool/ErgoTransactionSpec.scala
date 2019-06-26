package org.ergoplatform.modifiers.mempool

import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox._
import org.ergoplatform.nodeView.state.{ErgoStateContext, UpcomingStateContext, VotingData}
import org.ergoplatform.settings.Parameters.MaxBlockCostIncrease
import org.ergoplatform.settings.ValidationRules.{bsBlockTransactionsCost, txBoxSize}
import org.ergoplatform.settings._
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalacheck.Gen
import scalan.util.BenchmarkUtil
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base16
import sigmastate.Values.{ByteArrayConstant, ByteConstant, IntConstant, LongArrayConstant, SigmaPropConstant}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, CryptoConstants, ProverResult}

import scala.util.{Random, Try}

class ErgoTransactionSpec extends ErgoPropertyTest {

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)

  property("serialization vector") {
    // test vectors, that specifies transaction json and bytes representation.
    // ensures that bytes transaction representation was not changed

    def check(bytesStr: String, bytesToSign: String, jsonStr: String): Unit = {
      val bytes = Base16.decode(bytesStr).get
      val tx = ErgoTransactionSerializer.parseBytes(bytes)
      tx.asJson.noSpaces shouldBe jsonStr
      bytesToSign shouldBe Base16.encode(tx.messageToSign)
    }

    // simple transfer transaction with 2 inputs and 2 outputs (first is transfer, second is fee)
    val height = 1000
    val minerPkHex = "0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942"
    val minerPk = Base16.decode(minerPkHex).map { point =>
      ProveDlog(
        CryptoConstants.dlogGroup.curve.decodePoint(point).asInstanceOf[CryptoConstants.EcPointType]
      )
    }.get
    val inputs: IndexedSeq[Input] = IndexedSeq(
      new Input(ADKey @@ Base16.decode("c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742").get,
        new ProverResult(Base16.decode("b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de4674").get, new ContextExtension(Map()))),
      new Input(ADKey @@ Base16.decode("ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead").get,
        new ProverResult(Base16.decode("5aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340").get, new ContextExtension(Map()))),
    )
    val outputCandidates: IndexedSeq[ErgoBoxCandidate] = IndexedSeq(
      new ErgoBoxCandidate(1000000000L, minerPk, height, Colls.emptyColl, Map()),
      new ErgoBoxCandidate(1000000L, settings.chainSettings.monetary.feeProposition, height, Colls.emptyColl, Map())
    )
    val tx = ErgoTransaction(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate])

    val bytesToSign = "02c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f77420000ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead00000000028094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070000c0843d1005040004000e36100204cf0f08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304e8070000"
    val bytesStr = "02c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f774238b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de467400ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead385aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340000000028094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070000c0843d1005040004000e36100204cf0f08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304e8070000"
    val jsonStr = "{\"id\":\"b59ca51f7470f291acc32e84870d00c4fda8b773f38f757f3d65d45265c13da5\",\"inputs\":[{\"boxId\":\"c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742\",\"spendingProof\":{\"proofBytes\":\"b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de4674\",\"extension\":{}}},{\"boxId\":\"ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead\",\"spendingProof\":{\"proofBytes\":\"5aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340\",\"extension\":{}}}],\"dataInputs\":[],\"outputs\":[{\"boxId\":\"da288ce9e9a9d39f69634488a8d82c1bf4fb6ddce2f0930d2536016d8167eeb2\",\"value\":1000000000,\"ergoTree\":\"0008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942\",\"assets\":[],\"creationHeight\":1000,\"additionalRegisters\":{}},{\"boxId\":\"be609af4436111d5592dbd52bc64f6a46a1c0605fd30cd61c74850b7f9875762\",\"value\":1000000,\"ergoTree\":\"1005040004000e36100204cf0f08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304\",\"assets\":[],\"creationHeight\":1000,\"additionalRegisters\":{}}],\"size\":341}"
    Base16.encode(tx.bytes) shouldBe bytesStr

    check(bytesStr, bytesToSign, jsonStr)

    // tx with registers in outputs
    val outputCandidates2: IndexedSeq[ErgoBoxCandidate] = IndexedSeq(
      new ErgoBoxCandidate(1000000000L, minerPk, height, Colls.emptyColl,
        Map(
          R4 -> ByteConstant(1),
          R5 -> SigmaPropConstant(minerPk),
          R6 -> IntConstant(10),
          R7 -> LongArrayConstant(Array(1L, 2L, 1234123L)),
          R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
        )),
      new ErgoBoxCandidate(1000000000L, minerPk, height, Colls.emptyColl, Map())
    )
    val tx2 = ErgoTransaction(inputs: IndexedSeq[Input], outputCandidates2: IndexedSeq[ErgoBoxCandidate])

    Base16.encode(tx2.bytes) shouldBe "02c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f774238b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de467400ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead385aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340000000028094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070005020108cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b94204141103020496d396010e211234561234561234561234561234561234561234561234561234561234561234568094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070000"
    check(Base16.encode(tx2.bytes),
      "02c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f77420000ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead00000000028094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070005020108cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b94204141103020496d396010e211234561234561234561234561234561234561234561234561234561234561234568094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070000",
      "{\"id\":\"bd04a93f67fda77d89afc38cd8237f142ad5a349405929fd1f7b7f24c4ea2e80\",\"inputs\":[{\"boxId\":\"c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742\",\"spendingProof\":{\"proofBytes\":\"b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de4674\",\"extension\":{}}},{\"boxId\":\"ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead\",\"spendingProof\":{\"proofBytes\":\"5aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340\",\"extension\":{}}}],\"dataInputs\":[],\"outputs\":[{\"boxId\":\"1baffa8e5ffce634a8e70530023c16a5c177d2b5ab756ae89a8dce2a23ba433c\",\"value\":1000000000,\"ergoTree\":\"0008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942\",\"assets\":[],\"creationHeight\":1000,\"additionalRegisters\":{\"R5\":\"08cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942\",\"R6\":\"0414\",\"R8\":\"0e21123456123456123456123456123456123456123456123456123456123456123456\",\"R7\":\"1103020496d39601\",\"R4\":\"0201\"}},{\"boxId\":\"33eff46f94067b32073d5f81984607be559108f58bc3f53906a1e8db7cf0f708\",\"value\":1000000000,\"ergoTree\":\"0008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942\",\"assets\":[],\"creationHeight\":1000,\"additionalRegisters\":{}}],\"size\":356}")

    // tx with 2 inputs, 1 data input, 3 outputs with tokens 0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942
    check("02e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6400002e9798d7eb0cd867f6dc29872f80de64c04cef10a99a58d007ef7855f0acbdb9000001f97d1dc4626de22db836270fe1aa004b99970791e4557de8f486f6d433b81195026df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6403da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100fa979af8988ce7010008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000000",
      "02e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6400002e9798d7eb0cd867f6dc29872f80de64c04cef10a99a58d007ef7855f0acbdb9000001f97d1dc4626de22db836270fe1aa004b99970791e4557de8f486f6d433b81195026df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6403da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100fa979af8988ce7010008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000000",
      "{\"id\":\"663ae91ab7145a4f42b5509e1a2fb0469b7cb46ea87fdfd90e0b4c8ef29c2493\",\"inputs\":[{\"boxId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"spendingProof\":{\"proofBytes\":\"\",\"extension\":{}}},{\"boxId\":\"2e9798d7eb0cd867f6dc29872f80de64c04cef10a99a58d007ef7855f0acbdb9\",\"spendingProof\":{\"proofBytes\":\"\",\"extension\":{}}}],\"dataInputs\":[{\"boxId\":\"f97d1dc4626de22db836270fe1aa004b99970791e4557de8f486f6d433b81195\"}],\"outputs\":[{\"boxId\":\"69e05b68715caaa4ca58ba59a8c8c7e031d42ad890b05f87021a28617c1e70d5\",\"value\":524940416256346,\"ergoTree\":\"0008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176\",\"assets\":[{\"tokenId\":\"6df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963\",\"amount\":226153050},{\"tokenId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"amount\":536110126}],\"creationHeight\":0,\"additionalRegisters\":{}},{\"boxId\":\"556a9a3ec7880d468e56d44e75898cf8a32f6a07344895fa6b5cf34edf101a59\",\"value\":524940416256346,\"ergoTree\":\"0008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176\",\"assets\":[{\"tokenId\":\"6df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963\",\"amount\":226153050},{\"tokenId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"amount\":536110126}],\"creationHeight\":0,\"additionalRegisters\":{}},{\"boxId\":\"16385b5b83992629909c7e004ed0421229ed3587162ce6f29b2df129472e3909\",\"value\":1016367755463674,\"ergoTree\":\"0008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176\",\"assets\":[],\"creationHeight\":0,\"additionalRegisters\":{}}],\"size\":329}")
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
      checkTx(from, updateAnAsset(tx, from, _ + 1)) shouldBe 'failure
    }
  }

  property("impossible to create an asset of non-positive amount") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      checkTx(from, updateAnAsset(tx, from, _ => -1)) shouldBe 'failure
    }
  }

  property("impossible to overflow an asset value") {
    val gen = validErgoTransactionGenTemplate(1, 1, 8, 16)
    forAll(gen) { case (from, tx) =>
      val tokenOpt = tx.outputCandidates.flatMap(_.additionalTokens.toArray).map(t => ByteArrayWrapper.apply(t._1) -> t._2)
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
        checkTx(from, wrongTx) shouldBe 'failure
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
      e.getMessage should startWith(ValidationRules.errorMessage(ValidationRules.txScriptValidation, ""))
    }
  }

  property("assets usage correctly affects transaction total cost") {
    val txGen = validErgoTransactionGenTemplate(1, 1, 8, 16)
    forAll(txGen) { case (from, tx) =>
      val initTxCost = tx.statefulValidity(from, emptyDataBoxes, emptyStateContext).get

      // already existing token from one of the inputs
      val existingToken = from.flatMap(_.additionalTokens.toArray).toSet.head
      // completely new token
      val randomToken = (Digest32 @@ scorex.util.Random.randomBytes(), Random.nextInt(100000000).toLong)

      val in0 = from.last
      // new token added to the last input
      val modifiedIn0 = ErgoBox(in0.value, in0.ergoTree, in0.creationHeight,
        in0.additionalTokens.toArray.toSeq :+ randomToken, in0.additionalRegisters, in0.transactionId, in0.index)
      val txInMod0 = tx.inputs.last.copy(boxId = modifiedIn0.id)

      val in1 = from.last
      // existing token added to the last input
      val modifiedIn1 = ErgoBox(in1.value, in1.ergoTree, in1.creationHeight,
        in1.additionalTokens.toArray.toSeq :+ existingToken, in1.additionalRegisters, in1.transactionId, in1.index)
      val txInMod1 = tx.inputs.last.copy(boxId = modifiedIn1.id)

      val out0 = tx.outputs.last
      // new token added to the last output
      val modifiedOut0 = ErgoBox(out0.value, out0.ergoTree, out0.creationHeight,
        out0.additionalTokens.toArray.toSeq :+ randomToken, out0.additionalRegisters, out0.transactionId, out0.index)
      // existing token added to the last output
      val modifiedOut1 = ErgoBox(out0.value, out0.ergoTree, out0.creationHeight,
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
    val validFailure = txMod.statefulValidity(in, emptyDataBoxes, emptyStateContext)
    validFailure.failed.get.getMessage should startWith(ValidationRules.errorMessage(txBoxSize, "").take(30))

  }

  property("transaction with too many inputs should be rejected") {

    //we assume that verifier must finish verification of any script in less time than 500K hash calculations
    // (for the Blake2b256 hash function over a single block input)
    val Timeout: Long = {
      val block = Array.fill(16)(0: Byte)
      val hf = Blake2b256

      //just in case to heat up JVM
      (1 to 5000000).foreach(_ => hf(block))

      val t0 = System.currentTimeMillis()
      (1 to 500000).foreach(_ => hf(block))
      val t = System.currentTimeMillis()
      t - t0
    }

    val gen = validErgoTransactionGenTemplate(0, 0, 1000, 1000, trueLeafGen)
    val (from, tx) = gen.sample.get
    tx.statelessValidity.isSuccess shouldBe true

    //check that spam transaction is being rejected quickly
    implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)
    val (validity, time0) = BenchmarkUtil.measureTime(tx.statefulValidity(from, IndexedSeq(), emptyStateContext))
    validity.isSuccess shouldBe false
    assert(time0 <= Timeout)

    val cause = validity.failed.get.getMessage
    cause should startWith(ValidationRules.errorMessage(bsBlockTransactionsCost, "").take(30))

    //check that spam transaction validation with no cost limit is indeed taking too much time
    import Parameters._
    val ps = Parameters(0, DefaultParameters.updated(MaxBlockCostIncrease, Int.MaxValue), emptyVSUpdate)
    val sc = new ErgoStateContext(Seq.empty, None, genesisStateDigest, ps, ErgoValidationSettings.initial,
      VotingData.empty)(settings.chainSettings.voting)
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
    def stateContextWithMaxCost(manualCost: Int): UpcomingStateContext = {
      val table2: Map[Byte, Int] = Parameters.DefaultParameters + (MaxBlockCostIncrease -> (manualCost))
      val params2 = new Parameters(height = 0,
        parametersTable = table2,
        proposedUpdate = ErgoValidationSettingsUpdate.empty)
      emptyStateContext.copy(currentParameters = params2)
    }

    val gen = validErgoTransactionGenTemplate(0, 0, 10, 10, trueLeafGen)
    val (from, tx) = gen.sample.get
    tx.statelessValidity.isSuccess shouldBe true

    // calculate costs manually
    val initialCost: Long =
      tx.inputs.size * LaunchParameters.inputCost +
        tx.dataInputs.size * LaunchParameters.dataInputCost +
        tx.outputs.size * LaunchParameters.outputCost
    val (outAssets, outAssetsNum) = tx.outAssetsTry.get
    val (inAssets, inAssetsNum) = ErgoTransaction.extractAssets(from).get
    val totalAssetsAccessCost = (outAssetsNum + inAssetsNum) * LaunchParameters.tokenAccessCost +
      (inAssets.size + outAssets.size) * LaunchParameters.tokenAccessCost
    // todo replace 229 to references from CostTable
    val scriptsValidationCosts = tx.inputs.size * 229
    val manualCost: Int = (initialCost + totalAssetsAccessCost + scriptsValidationCosts).toInt


    // check that validation pass if cost limit equals to manually calculated cost
    val sc = stateContextWithMaxCost(manualCost)
    sc.currentParameters.maxBlockCost shouldBe manualCost
    val calculatedCost = tx.statefulValidity(from, IndexedSeq(), sc)(ErgoInterpreter(sc.currentParameters)).get
    manualCost shouldBe calculatedCost

    // transaction exceeds computations limit
    val sc2 = stateContextWithMaxCost(manualCost - 1)
    tx.statefulValidity(from, IndexedSeq(), sc2)(ErgoInterpreter(sc2.currentParameters)) shouldBe 'failure

    // transaction exceeds computations limit due to non-zero accumulated cost
    tx.statefulValidity(from, IndexedSeq(), sc, 1)(ErgoInterpreter(sc.currentParameters)) shouldBe 'failure

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

  private def checkTx(from: IndexedSeq[ErgoBox], wrongTx: ErgoTransaction): Try[Long] = {
    wrongTx.statelessValidity.flatMap(_ => wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext))
  }

}
