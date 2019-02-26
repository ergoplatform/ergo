package org.ergoplatform.modifiers.mempool

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import org.ergoplatform.settings.LaunchParameters
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.crypto.hash.Digest32
import sigmastate._
import sigmastate.Values.GroupElementConstant
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.CryptoConstants
import scala.collection.JavaConverters._

import scala.util.Random

class ErgoTransactionSpec extends ErgoPropertyTest {

  private def modifyValue(boxCandidate: ErgoBoxCandidate, delta: Long): ErgoBoxCandidate = {
    new ErgoBoxCandidate(
      boxCandidate.value + delta,
      boxCandidate.proposition,
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
      boxCandidate.proposition,
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
          new ErgoBoxCandidate(c.value, c.proposition, startHeight, updTokens, c.additionalRegisters)
        }

        val wrongTx = tx.copy(outputCandidates = updCandidates)
        wrongTx.statelessValidity.isSuccess shouldBe false
        wrongTx.statefulValidity(from, emptyStateContext).isSuccess shouldBe false
      }
    }
  }

  property("stateful validation should catch false proposition") {
    val propositionGen = Gen.const(Values.FalseLeaf)
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
    val txGen = validErgoTransactionGenTemplate(10, 100)
    forAll(txGen) { case (from, tx) =>
      val initTxCost = tx.statefulValidity(from, emptyStateContext).get
      val additionalToken = (Digest32 @@ scorex.util.Random.randomBytes(), Random.nextInt(100000000).toLong)
      val in0 = from.head
      val modifiedIn0 = ErgoBox(in0.value, in0.proposition, in0.creationHeight,
        in0.additionalTokens :+ additionalToken, in0.additionalRegisters, in0.transactionId, in0.index)
      val txInMod0 = tx.inputs.head.copy(boxId = modifiedIn0.id)
      val out0 = tx.outputs.head
      val modifiedOut0 = ErgoBox(out0.value, out0.proposition, out0.creationHeight,
        out0.additionalTokens :+ additionalToken, out0.additionalRegisters, out0.transactionId, out0.index)
      val txMod0 = tx.copy(inputs = txInMod0 +: tx.inputs.tail, outputCandidates = modifiedOut0 +: tx.outputCandidates.tail)
      val inputIncTxCost = txMod0.statefulValidity(modifiedIn0 +: from.tail, emptyStateContext).get
    }
  }

  private def groupElemGen =
    Gen.const(GroupElementConstant(CryptoConstants.dlogGroup.createRandomGenerator()))

  private def proveDiffieHellmanTupleGen = for {
    gv <- groupElemGen
    hv <- groupElemGen
    uv <- groupElemGen
    vv <- groupElemGen
  } yield ProveDHTuple(gv, hv, uv, vv)

  ignore("too costly transaction should be rejected") {
    val propositionGen = for {
      proveList <- Gen.listOfN(50, proveDiffieHellmanTupleGen)
    } yield OR(proveList)

    val gen = validErgoTransactionGenTemplate(1, 1, 1, 1, propositionGen)

    forAll(gen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      val validity = tx.statefulValidity(from, emptyStateContext)
      validity.isSuccess shouldBe false
      val cause = validity.failed.get.getCause
      Option(cause) shouldBe defined
      cause.getMessage should startWith("Estimated expression complexity")
    }
  }

}
