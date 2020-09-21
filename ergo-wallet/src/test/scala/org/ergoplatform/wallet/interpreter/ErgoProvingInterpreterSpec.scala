package org.ergoplatform.wallet.interpreter

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, UnsignedErgoLikeTransaction, UnsignedInput}
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter.TransactionHintsBag
import org.ergoplatform.wallet.secrets.{DlogSecretKey, ExtendedSecretKey}
import org.ergoplatform.wallet.utils.Generators
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import sigmastate.interpreter.{ContextExtension, HintsBag}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import scorex.util.Random
import sigmastate.CTHRESHOLD
import sigmastate.Values.{GroupElementConstant, SigmaBoolean}
import sigmastate.serialization.ErgoTreeSerializer


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
      val signedTxFull = fullProver.sign(unsignedTx, ins.toIndexedSeq, IndexedSeq(),
        stateContext, TransactionHintsBag.empty).get
      val signedTxUnsafe = primitiveProver.sign(unsignedTx, ins.toIndexedSeq, IndexedSeq(),
        stateContext, TransactionHintsBag.empty).get

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

    val hintsForAlice = HintsBag(Seq(cmtHint))
    val txHintsForAlice = TransactionHintsBag(Map(0 -> hintsForAlice))
    val signRes = prover1.sign(utx, IndexedSeq(inputBox), IndexedSeq(), stateContext, txHintsForAlice)
    signRes.isSuccess shouldBe true

    val hints = prover1
      .bagForTransaction(signRes.get, IndexedSeq(inputBox), IndexedSeq(), stateContext, Seq(pk1), Seq(pk2))

    val txHintsForBob = TransactionHintsBag(Map(0 -> HintsBag(Seq(ownCmt))), Map(0 -> hints.publicHints(0)))

    val signedTxTry = prover0.sign(utx, IndexedSeq(inputBox), IndexedSeq(), stateContext, txHintsForBob)
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
    val signRes = prover.sign(utx, inputBoxes, IndexedSeq(), stateContext, TransactionHintsBag.empty)
    signRes.isSuccess shouldBe true
  }

  it should "produce hints" in {
    import ErgoBox._
    import sigmastate.eval._

    val prover = ErgoProvingInterpreter(obtainSecretKey(), parameters)
    val pk = prover.hdPubKeys.head.key

    val pk2 = obtainSecretKey().publicKey.key
    val pk3 = obtainSecretKey().publicKey.key

    val ergoTreeBytes = Base16.decode("10010404987300830308cde4c6a70407cde4c6a70507cde4c6a70607").get
    val ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(ergoTreeBytes)

    val registers = Map(
      R4 -> GroupElementConstant(CGroupElement(pk.value)),
      R5 -> GroupElementConstant(CGroupElement(pk2.value)),
      R6 -> GroupElementConstant(CGroupElement(pk3.value))
    )

    val transactionId = ModifierId @@ Base16.encode(Array.fill(32)(5: Byte))

    val value = 1000000
    val input = new ErgoBox(value, ergoTree, Colls.emptyColl[(TokenId, Long)], registers, transactionId, 0, 1)


    val utx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(input.id, ContextExtension.empty)),
      IndexedSeq(input.toCandidate)
    )

    val thb = prover.generateCommitmentsFor(utx, IndexedSeq(input), IndexedSeq.empty, stateContext).get

    thb.secretHints(0).hints.size shouldBe 1

    thb.publicHints(0).hints.size shouldBe 1
  }

}
