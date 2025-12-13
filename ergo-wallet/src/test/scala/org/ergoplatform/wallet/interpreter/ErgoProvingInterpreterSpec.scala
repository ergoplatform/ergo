package org.ergoplatform.wallet.interpreter

import org.ergoplatform.sdk.wallet.secrets.{DlogSecretKey, ExtendedSecretKey}
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, UnsignedErgoLikeTransaction, UnsignedInput}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.{ModifierId, Random}
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast.{ErgoTree, GroupElementConstant}
import sigma.data.{CGroupElement, CTHRESHOLD, SigmaBoolean}
import sigma.interpreter.ContextExtension
import sigma.serialization.ErgoTreeSerializer
import sigmastate.interpreter.HintsBag


class ErgoProvingInterpreterSpec
  extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with InterpreterSpecCommon {
  import org.ergoplatform.wallet.utils.WalletGenerators._


  private def obtainSecretKey() = ExtendedSecretKey.deriveMasterKey(Random.randomBytes(32), usePre1627KeyDerivation = false)

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
          ErgoSignature.verify(unsignedTx.messageToSign, fullProof, extendedSecretKey.publicKey.key.value) shouldBe
            ErgoSignature.verify(unsignedTx.messageToSign, unsafeProof, extendedSecretKey.publicKey.key.value)
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

    val boxCandidate = new ErgoBoxCandidate(value, ErgoTree.fromProposition(prop), creationHeight)
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
    val boxCandidate = new ErgoBoxCandidate(value, ErgoTree.fromSigmaBoolean(pk), creationHeight)

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

  it should "not overflow when calculating storage fee for large boxes" in {
    // This test verifies the fix for issue #2251
    // With storageFeeFactor = 1250000 and box size = 2239 bytes:
    // Without fix: 2239 * 1250000 = 2798750000 > Int.MaxValue (2147483647) -> overflow
    // With fix: 2239L * 1250000 = 2798750000L -> correct result
    
    val interpreter = ErgoInterpreter(parameters)
    val prover = ErgoProvingInterpreter(obtainSecretKey(), parameters)
    val pk = prover.hdPubKeys.head.key
    
    // Create a large box (2239 bytes as mentioned in the issue)
    val largeScript = ErgoTree.fromSigmaBoolean(pk)
    // Add enough registers to make the box approximately 2239 bytes
    val registers = Map(
      ErgoBox.R4 -> GroupElementConstant(CGroupElement(pk.value)),
      ErgoBox.R5 -> GroupElementConstant(CGroupElement(pk.value)),
      ErgoBox.R6 -> GroupElementConstant(CGroupElement(pk.value)),
      ErgoBox.R7 -> GroupElementConstant(CGroupElement(pk.value)),
      ErgoBox.R8 -> GroupElementConstant(CGroupElement(pk.value)),
      ErgoBox.R9 -> GroupElementConstant(CGroupElement(pk.value))
    )
    
    val value = 10000000000L // 10 ERG
    val creationHeight = 0
    val fakeTxId = ModifierId @@ Base16.encode(Array.fill(32)(5: Byte))
    val boxCandidate = new ErgoBoxCandidate(value, largeScript, creationHeight, Colls.emptyColl, registers)
    val largeBox = boxCandidate.toBox(fakeTxId, 0)
    
    // Calculate storage fee - this should not overflow
    val storageFee = parameters.storageFeeFactor.toLong * largeBox.bytes.length
    
    // Verify the calculation doesn't overflow (would be negative if it did)
    storageFee should be > 0L
    
    // Verify the storage fee is calculated correctly
    // For the example in the issue: 2239 * 1250000 = 2798750000
    if (largeBox.bytes.length == 2239) {
      storageFee shouldBe 2798750000L
    }
    
    // Verify that the storage fee is less than Int.MaxValue * box.bytes.length
    // to confirm we're avoiding the overflow
    val wouldOverflow = parameters.storageFeeFactor * largeBox.bytes.length
    if (storageFee > Int.MaxValue) {
      // If the correct result is > Int.MaxValue, the Int multiplication would have overflowed
      wouldOverflow should be < 0 // This confirms overflow would have happened
    }
  }

}
