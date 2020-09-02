package org.ergoplatform.wallet.interpreter

import org.ergoplatform.{ErgoBoxCandidate, UnsignedErgoLikeTransaction, UnsignedInput}
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.wallet.secrets.{DlogSecretKey, ExtendedSecretKey}
import org.ergoplatform.wallet.utils.Generators
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import sigmastate.interpreter.{ContextExtension, HintsBag}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import scorex.util.Random
import sigmastate.CTHRESHOLD
import sigmastate.Values.SigmaBoolean

class ErgoProvingInterpreterSpec
  extends FlatSpec
    with PropertyChecks
    with Matchers
    with Generators
    with InterpreterSpecCommon {

  private def obtainSecretKey() = ExtendedSecretKey.deriveMasterKey(Random.randomBytes(32))

  it should "produce proofs with primitive secrets" in {
    val extendedSecretKey = obtainSecretKey()
    val fullProver = ErgoProvingInterpreter(extendedSecretKey, parameters)

    val primitiveKey = DlogSecretKey(extendedSecretKey.privateInput)
    val primitiveProver = ErgoProvingInterpreter(IndexedSeq(primitiveKey), parameters)

    forAll(unsignedTxGen(extendedSecretKey)) { case (ins, unsignedTx) =>
      val signedTxFull = fullProver.sign(unsignedTx, ins.toIndexedSeq, IndexedSeq(), stateContext).get
      val signedTxUnsafe = primitiveProver.sign(unsignedTx, ins.toIndexedSeq, IndexedSeq(), stateContext).get

      signedTxFull shouldEqual signedTxUnsafe

      signedTxFull.inputs.map(_.spendingProof.proof).zip(signedTxFull.inputs.map(_.spendingProof.proof))
        .foreach { case (fullProof, unsafeProof) =>
          ErgoSignature.verify(unsignedTx.messageToSign, fullProof, extendedSecretKey.publicKey.key.h) shouldBe
            ErgoSignature.verify(unsignedTx.messageToSign, unsafeProof, extendedSecretKey.publicKey.key.h)
        }
    }
  }

  it should "produce a signature with enough hints given - 2-out-of-3 case" in {
    val prover0 = ErgoProvingInterpreter(obtainSecretKey(), parameters) // real
    val prover1 = ErgoProvingInterpreter(obtainSecretKey(), parameters) // real
    val prover2 = ErgoProvingInterpreter(obtainSecretKey(), parameters) // simulated

    val pk0 = prover0.hdPubKeys.head.key
    val pk1 = prover1.hdPubKeys.head.key
    val pk2 = prover2.hdPubKeys.head.key

    val prop: SigmaBoolean = CTHRESHOLD(2, Seq(pk0, pk1, pk2))

    val value = 100000000L

    val creationHeight = 10000

    val boxCandidate = new ErgoBoxCandidate(value, prop, creationHeight)
    val fakeTxId = ModifierId @@ Base16.encode(Array.fill(32)(5: Byte))
    val inputBox = boxCandidate.toBox(fakeTxId, 0.toShort)

    val unsignedInput = new UnsignedInput(inputBox.id, ContextExtension.empty)

    val utx = new UnsignedErgoLikeTransaction(IndexedSeq(unsignedInput), IndexedSeq.empty, IndexedSeq(boxCandidate))

    val aliceBag = prover0.generateCommitments(prop)
    val (cmtHint, ownCmt) = (aliceBag.realCommitments.head, aliceBag.ownCommitments.head)

    val signRes = prover1.withHints(HintsBag(Seq(cmtHint))).sign(utx, IndexedSeq(inputBox), IndexedSeq(), stateContext)
    signRes.isSuccess shouldBe true

    val hints = prover1
      .bagForTransaction(signRes.get, IndexedSeq(inputBox), IndexedSeq(), stateContext, Seq(pk1), Seq(pk2))
      .addHint(ownCmt)

    val signedTxTry = prover0.withHints(hints).sign(utx, IndexedSeq(inputBox), IndexedSeq(), stateContext)
    signedTxTry.isSuccess shouldBe true
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
