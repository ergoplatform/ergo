package org.ergoplatform.wallet.interpreter

import org.ergoplatform.{ErgoBoxCandidate, UnsignedErgoLikeTransaction, UnsignedInput}
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.wallet.secrets.{DlogSecretKey, ExtendedSecretKey}
import org.ergoplatform.wallet.utils.Generators
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.interpreter.ContextExtension
import scorex.util.Random

class ErgoProvingInterpreterSpec
  extends FlatSpec
    with PropertyChecks
    with Matchers
    with Generators
    with InterpreterSpecCommon {

  private def obtainSecretKey() = ExtendedSecretKey.deriveMasterKey(Random.randomBytes(32))

  it should "produce proofs with primitive secrets" in {
    val entropy = Random.randomBytes(32)
    val extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy)
    val fullProver = ErgoProvingInterpreter(extendedSecretKey, parameters)

    val primitiveKey = DlogSecretKey(extendedSecretKey.privateInput)
    val unsafeProver = ErgoProvingInterpreter(IndexedSeq(primitiveKey), parameters)

    forAll(unsignedTxGen(extendedSecretKey)) { case (ins, unsignedTx) =>
      val signedTxFull = fullProver.sign(unsignedTx, ins.toIndexedSeq, IndexedSeq(), stateContext).get
      val signedTxUnsafe = unsafeProver.sign(unsignedTx, ins.toIndexedSeq, IndexedSeq(), stateContext).get

      signedTxFull shouldEqual signedTxUnsafe

      signedTxFull.inputs.map(_.spendingProof.proof).zip(signedTxFull.inputs.map(_.spendingProof.proof))
        .foreach { case (fullProof, unsafeProof) =>
          ErgoSignature.verify(unsignedTx.messageToSign, fullProof, extendedSecretKey.publicKey.key.h) shouldBe
            ErgoSignature.verify(unsignedTx.messageToSign, unsafeProof, extendedSecretKey.publicKey.key.h)
        }
    }
  }

  it should "sign 50 simple inputs with default cost limit" in {
    val prover = ErgoProvingInterpreter(obtainSecretKey(), parameters)
    val pk = prover.hdPubKeys.head.key

    val value = 100000000L
    val creationHeight = 10000
    val boxCandidate = new ErgoBoxCandidate(value, pk, creationHeight)

    val numOfInputs = 50
    val fakeTxId = ModifierId @@ Base16.encode(Array.fill(32)(5: Byte))
    val inputBoxes = (1 to numOfInputs).map(i => boxCandidate.toBox(fakeTxId, i.toShort))
    val unsignedInputs = inputBoxes.map(ib => new UnsignedInput(ib.id, ContextExtension.empty))

    val utx = new UnsignedErgoLikeTransaction(unsignedInputs, IndexedSeq.empty, IndexedSeq(boxCandidate))
    val signRes = prover.sign(utx, inputBoxes, IndexedSeq(), stateContext)
    signRes.isSuccess shouldBe true
  }

}
