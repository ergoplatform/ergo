package org.ergoplatform.wallet.interpreter

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast.{ByteArrayConstant, ErgoTree, EvaluatedValue, FalseLeaf, SType}
import sigma.eval.Extensions._

class StorageRentOverflowSpec
  extends AnyFlatSpec
    with Matchers
    with InterpreterSpecCommon {

  private class TestInterpreter extends ErgoInterpreter(parameters) {
    def checkExpired(box: ErgoBox, output: ErgoBoxCandidate, currentHeight: Int): Boolean =
      checkExpiredBox(box, output, currentHeight)
  }

  it should "accept covered storage-rent recreation when the byte fee is larger than Int.MaxValue" in {
    val transactionId = ModifierId @@ Base16.encode(Array.fill(32)(5: Byte))
    val registers: Map[ErgoBox.NonMandatoryRegisterId, EvaluatedValue[_ <: SType]] =
      Map(ErgoBox.R4 -> ByteArrayConstant(Array.fill(2200)(1: Byte)))
    val input = new ErgoBox(
      value = 10000000000L,
      ergoTree = ErgoTree.fromProposition(FalseLeaf.toSigmaProp),
      additionalTokens = Colls.emptyColl[(ErgoBox.TokenId, Long)],
      additionalRegisters = registers,
      transactionId = transactionId,
      index = 0,
      creationHeight = 1
    )

    val storageFee = parameters.storageFeeFactor.toLong * input.bytes.length
    storageFee should be > Int.MaxValue.toLong

    val currentHeight = 2000000
    val recreated = new ErgoBoxCandidate(
      value = input.value - storageFee,
      ergoTree = input.ergoTree,
      creationHeight = currentHeight,
      additionalTokens = input.additionalTokens,
      additionalRegisters = input.additionalRegisters
    )

    new TestInterpreter().checkExpired(input, recreated, currentHeight) shouldBe true
  }
}
