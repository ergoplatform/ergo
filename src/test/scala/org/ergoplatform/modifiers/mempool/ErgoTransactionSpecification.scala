package org.ergoplatform.modifiers.mempool

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scapi.sigma.ProveDiffieHellmanTuple
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigmastate._
import sigmastate.Values.GroupElementConstant
import sigmastate.interpreter.CryptoConstants

import scala.util.Random

class ErgoTransactionSpecification extends ErgoPropertyTest {

  private val context = ErgoStateContext(0, ADDigest @@ Array.fill(32)(0: Byte))

  private def modifyValue(boxCandidate: ErgoBoxCandidate, delta: Long): ErgoBoxCandidate = {
    new ErgoBoxCandidate(
      boxCandidate.value + delta,
      boxCandidate.proposition,
      boxCandidate.additionalTokens,
      boxCandidate.additionalRegisters)
  }

  private def modifyAsset(boxCandidate: ErgoBoxCandidate,
                          deltaFn: Long => Long,
                          idToskip: TokenId): ErgoBoxCandidate = {
    val assetId = boxCandidate.additionalTokens.find(t => !java.util.Arrays.equals(t._1, idToskip)).get._1

    val tokens = boxCandidate.additionalTokens.map { case (id, amount) =>
      if (id.sameElements(assetId)) assetId -> deltaFn(amount) else assetId -> amount
    }

    new ErgoBoxCandidate(
      boxCandidate.value,
      boxCandidate.proposition,
      tokens,
      boxCandidate.additionalRegisters)
  }

  property("a valid transaction is valid") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(from, context).isSuccess shouldBe true
    }
  }

  property("ergo preservation law holds") {
    forAll(validErgoTransactionGen, smallPositiveInt) { case ((from, tx), deltaAbs) =>
      val delta = if (Random.nextBoolean()) -deltaAbs else deltaAbs

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, delta) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess shouldBe true
      wrongTx.statefulValidity(from, context).isSuccess shouldBe false
    }
  }

  property("impossible to create a negative-value output") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val negValue = Math.min(Math.abs(Random.nextLong()), Long.MaxValue - tx.outputCandidates.head.value)
      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, -(tx.outputCandidates.head.value + negValue)) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, context).isSuccess shouldBe false
    }
  }

  property("impossible to overflow ergo tokens") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val overflowSurplus = (Long.MaxValue - tx.outputCandidates.map(_.value).sum) + 1

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, overflowSurplus) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, context).isSuccess shouldBe false
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
      wrongTx.statefulValidity(from, context).isSuccess shouldBe false
    }
  }

  property("impossible to create an asset of non-positive amount") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      val wrongTx = updateAnAsset(tx, from, _ => -1)
      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, context).isSuccess shouldBe false
    }
  }

  property("impossible to overflow an asset value") {
    val gen = validErgoTransactionGenTemplate(1, 1, 8, 16)
    forAll(gen){ case (from, tx) =>
      val tokenOpt = tx.outputCandidates.flatMap(_.additionalTokens).map(t => ByteArrayWrapper.apply(t._1) -> t._2)
        .groupBy(_._1).find(_._2.size >= 2)

      whenever(tokenOpt.nonEmpty){
        val tokenId = tokenOpt.get._1
        val tokenAmount = tokenOpt.get._2.map(_._2).sum

        var modified = false
        val updCandidates = tx.outputCandidates.map { c =>
          val updTokens = c.additionalTokens.map{case (id, amount) =>
            if(!modified && ByteArrayWrapper(id) == tokenId){
              modified = true
              id -> ((Long.MaxValue - tokenAmount) + amount + 1)
            } else {
              id -> amount
            }
          }
          new ErgoBoxCandidate(c.value, c.proposition, updTokens, c.additionalRegisters)
        }

        val wrongTx = tx.copy(outputCandidates = updCandidates)
        wrongTx.statelessValidity.isSuccess shouldBe false
        wrongTx.statefulValidity(from, context).isSuccess shouldBe false
      }
    }
  }

  property("stateful validation should catch false proposition") {
    val propositionGen = Gen.const(Values.FalseLeaf)
    val gen = validErgoTransactionGenTemplate(1, 1, 1, 1, propositionGen)
    forAll(gen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      val validity = tx.statefulValidity(from, context)
      validity.isSuccess shouldBe false
      validity.failed.get.getMessage should startWith("Validation failed for input #0")
    }
  }

  private def groupElemGen =
    Gen.const(GroupElementConstant(CryptoConstants.dlogGroup.createRandomGenerator()))

  private def proveDiffieHellmanTupleGen = for {
    gv <- groupElemGen
    hv <- groupElemGen
    uv <- groupElemGen
    vv <- groupElemGen
  } yield ProveDiffieHellmanTuple(gv, hv, uv, vv)

  property("too long expressions should be rejected") {
    val propositionGen = for {
      proveList <- Gen.listOfN(350, proveDiffieHellmanTupleGen)
    } yield OR(proveList)

    val gen = validErgoTransactionGenTemplate(1, 1, 1, 1, propositionGen)

    forAll(gen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      val validity = tx.statefulValidity(from, context)
      validity.isSuccess shouldBe false
      val cause = validity.failed.get.getCause
      Option(cause) shouldBe defined
      cause.getMessage should startWith("requirement failed: Too long expression")
    }
  }

  property("too costly transaction should be rejected") {
    val propositionGen = for {
      proveList <- Gen.listOfN(50, proveDiffieHellmanTupleGen)
    } yield OR(proveList)

    val gen = validErgoTransactionGenTemplate(1, 1, 1, 1, propositionGen)

    forAll(gen) { case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      val validity = tx.statefulValidity(from, context)
      validity.isSuccess shouldBe false
      val cause = validity.failed.get.getCause
      Option(cause) shouldBe defined
      cause.getMessage should startWith("Estimated expression complexity")
    }
  }

  property("output contains too many assets") {
    val gen = validErgoTransactionGenTemplate(1, 2, 1, 2)
    forAll(gen) { case (from, tx) =>
      var modified = false
      val updCandidates = tx.outputCandidates.map { c =>
        if(!modified) {
          c.additionalTokens.find(_._2 > ErgoBox.MaxTokens + 1 - c.additionalTokens.size) match {
            case Some((assetId, amount)) =>
              val updAmount = amount - (ErgoBox.MaxTokens + 1)
              val updTokens = Seq(assetId -> amount) ++ (1 to (amount - updAmount).toInt).map(_ => assetId -> 1L) ++
                c.additionalTokens.filterNot(t => java.util.Arrays.equals(t._1, assetId))
                modified = true
              new ErgoBoxCandidate(c.value, c.proposition, updTokens, c.additionalRegisters)
            case None => c
          }
        } else {
          c
        }
      }
      val wrongTx = tx.copy(outputCandidates = updCandidates)
      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, context).isSuccess shouldBe false
    }
  }

  property("tx outputs contain too many assets in total") {
    val gen = validErgoTransactionGenTemplate(
      ErgoTransaction.MaxTokens + 1,
      ErgoTransaction.MaxTokens + 4,
      ErgoTransaction.MaxTokens + 4,
      ErgoTransaction.MaxTokens + 8)
    forAll(gen){ case (from, wrongTx) =>
      wrongTx.statelessValidity.isSuccess shouldBe false
      wrongTx.statefulValidity(from, context).isSuccess shouldBe false
    }
  }
}
