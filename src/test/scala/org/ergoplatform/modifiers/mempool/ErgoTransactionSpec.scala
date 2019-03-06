package org.ergoplatform.modifiers.mempool

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.settings.{Constants, LaunchParameters, Parameters}
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import org.scalacheck.Gen
import scorex.crypto.hash.Digest32

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

  property("a valid transaction is valid") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(from, emptyStateContext).isSuccess shouldBe true
    }
  }

  property("ergo preservation law holds") {
    forAll(validErgoTransactionGen, smallPositiveInt) { case ((from, tx), deltaAbs) =>
      val delta = if (Random.nextBoolean()) -deltaAbs else deltaAbs

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, delta) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess &&
        wrongTx.statefulValidity(from, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to create a negative-value output") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val negValue = Math.min(Math.abs(Random.nextLong()), Long.MaxValue - tx.outputCandidates.head.value)
      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, -(tx.outputCandidates.head.value + negValue)) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to overflow ergo tokens") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val overflowSurplus = (Long.MaxValue - tx.outputCandidates.map(_.value).sum) + 1

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, overflowSurplus) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyStateContext).isSuccess shouldBe false
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
      wrongTx.statefulValidity(from, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to create an asset of non-positive amount") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      val wrongTx = updateAnAsset(tx, from, _ => -1)
      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyStateContext).isSuccess shouldBe false
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
        wrongTx.statefulValidity(from, emptyStateContext).isSuccess shouldBe false
      }
    }
  }

  property("stateful validation should catch false proposition") {
    val propositionGen = Gen.const(Constants.FalseLeaf)
    val gen = validErgoTransactionGenTemplate(1, 1, 1, 1, propositionGen)
    forAll(gen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      val validity = tx.statefulValidity(from, emptyStateContext)
      validity.isSuccess shouldBe false
      val e = validity.failed.get
      log.info(s"Validation message: ${e.getMessage}", e)
      e.getMessage should startWith("Input script verification failed for input #0")
    }
  }

  property("assets usage correctly affects transaction total cost") {
    val txGen = validErgoTransactionGenTemplate(1, 1, 8, 16)
    forAll(txGen) { case (from, tx) =>
      val initTxCost = tx.statefulValidity(from, emptyStateContext).get

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

      val inputIncTxCost0 = txMod0.statefulValidity(from.init :+ modifiedIn0, emptyStateContext).get
      val inputIncTxCost1 = txMod1.statefulValidity(from.init :+ modifiedIn1, emptyStateContext).get
      val outputIncTxCost0 = txMod2.statefulValidity(from.init :+ modifiedIn0, emptyStateContext).get
      val outputIncTxCost1 = txMod3.statefulValidity(from.init :+ modifiedIn1, emptyStateContext).get

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
    val cost = txMod.statefulValidity(in, emptyStateContext).get
    cost shouldBe > (LaunchParameters.maxBlockCost)
  }

  ignore("too costly transaction should be rejected") {
/*
    todo fix or remove
    val groupElemGen: Gen[EcPointType] = Gen.const(CryptoConstants.dlogGroup.createRandomGenerator())

    val proveDiffieHellmanTupleGen = for {
      gv <- groupElemGen
      hv <- groupElemGen
      uv <- groupElemGen
      vv <- groupElemGen
    } yield ProveDHTuple(gv, hv, uv, vv)


    val propositionGen = for {
      proveList <- Gen.listOfN(50, proveDiffieHellmanTupleGen)
    } yield OR(proveList.map(_.toSigmaProp))

    val gen = validErgoTransactionGenTemplate(1, 1, 1, 1, propositionGen)

    forAll(gen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      val validity = tx.statefulValidity(from, emptyStateContext)
      validity.isSuccess shouldBe false
      val cause = validity.failed.get.getCause
      Option(cause) shouldBe defined
      cause.getMessage should startWith("Estimated expression complexity")
    }
*/
  }

}
