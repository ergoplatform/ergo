package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest

import scala.util.Random

class ErgoTransactionSpecification extends ErgoPropertyTest {

  private val context = ErgoStateContext(0, ADDigest @@ Array.fill(32)(0:Byte))

  private def modifyValue(boxCandidate: ErgoBoxCandidate, delta: Long): ErgoBoxCandidate = {
    new ErgoBoxCandidate(
      boxCandidate.value + delta,
      boxCandidate.proposition,
      boxCandidate.additionalTokens,
      boxCandidate.additionalRegisters)
  }

  property("a valid transaction is valid") {
    forAll(validErgoTransactionGen){case (from, tx) =>
      tx.statelessValidity.isSuccess shouldBe true
      tx.statefulValidity(from, context).isSuccess shouldBe true
    }
  }

  property("ergo preservation law holds") {
    forAll(validErgoTransactionGen, smallPositiveInt) { case ((from, tx), deltaAbs) =>
      val delta = if(Random.nextBoolean()) -deltaAbs else deltaAbs

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

  property("assets preservation law holds") {

  }

  property("impossible to create a negative-value asset") {

  }

  property("impossible to overflow an asset value") {

  }

  property("too costly transaction is rejected") {

  }

  property("output contains too many assets") {

  }

  property("tx outputs contain too many assets") {

  }
}