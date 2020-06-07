package org.ergoplatform.wallet.interpreter

import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransaction, Input, UnsignedErgoLikeTransaction, UnsignedInput}
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.wallet.secrets.{DlogSecretKey, ExtendedSecretKey}
import org.ergoplatform.wallet.utils.Generators
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scorex.util.{ModifierId, Random}
import scorex.util.encode.Base16
import sigmastate.CTHRESHOLD
import sigmastate.interpreter.{ContextExtension, HintsBag, OwnCommitment, RealCommitment}

class ErgoProvingInterpreterSpec
  extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with Generators
    with InterpreterSpecCommon {

  it should "produce proofs with primitive secrets" in {
    val entropy = Random.randomBytes(32)
    val extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy)
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
    def obtainSecretKey() = ExtendedSecretKey.deriveMasterKey(Random.randomBytes(32))

    val prover0 = ErgoProvingInterpreter(obtainSecretKey(), parameters) // real
    val prover1 = ErgoProvingInterpreter(obtainSecretKey(), parameters) // real
    val prover2 = ErgoProvingInterpreter(obtainSecretKey(), parameters) // simulated

    val pk0 = prover0.hdPubKeys.head
    val pk1 = prover1.hdPubKeys.head
    val pk2 = prover2.hdPubKeys.head

    val prop = CTHRESHOLD(2, Seq(pk0, pk1, pk2))

    val value = 100000000L

    val creationHeight = 10000

    val boxCandidate = new ErgoBoxCandidate(value, prop, creationHeight)
    val fakeTxId = ModifierId @@ Base16.encode(Array.fill(32)(5: Byte))
    val inputBox = boxCandidate.toBox(fakeTxId, 0.toShort)

    val unsignedInput = new UnsignedInput(inputBox.id, ContextExtension.empty)

    val tx = new UnsignedErgoLikeTransaction(IndexedSeq(unsignedInput), IndexedSeq.empty, IndexedSeq(boxCandidate))

    val (r, a) = ErgoProvingInterpreter.generateCommitmentFor(pk0)
    val cmtHint = RealCommitment(pk0, a)

    val signRes = prover1.withHints(HintsBag(Seq(cmtHint))).sign(tx, IndexedSeq(inputBox), IndexedSeq(), stateContext)
    signRes.isSuccess shouldBe true

    val hints = prover1
      .bagForTransaction(signRes.get, IndexedSeq(inputBox), IndexedSeq(), stateContext, Seq(pk1), Seq(pk2))
      .addHint(OwnCommitment(pk0, r, a))

    val signedTx = prover0.withHints(hints).sign(tx, IndexedSeq(inputBox), IndexedSeq(), stateContext).get
  }
}
