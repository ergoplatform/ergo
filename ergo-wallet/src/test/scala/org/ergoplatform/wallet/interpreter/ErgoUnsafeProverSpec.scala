package org.ergoplatform.wallet.interpreter

import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext}
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.wallet.utils.Generators
import org.ergoplatform._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scorex.crypto.authds.ADDigest
import scorex.util.Random
import scorex.util.encode.Base16
import sigmastate.Values.{ErgoTree, SigmaPropValue}
import sigmastate.basics.DLogProtocol.ProveDlog
import special.sigma._
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.CryptoConstants
import special.collection.Coll
import special.sigma.{Header, PreHeader}

class ErgoUnsafeProverSpec
  extends FlatSpec
    with GeneratorDrivenPropertyChecks
    with Matchers
    with Generators {

  it should "produce the same proof as a fully-functional prover" in {
    val entropy = Random.randomBytes(32)
    val extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy)
    val fullProver = ErgoProvingInterpreter(extendedSecretKey, parameters)
    val unsafeProver = ErgoUnsafeProver
    forAll(Gen.listOfN(2, ergoBoxGen(propGen(extendedSecretKey.publicKey.key))),
      Gen.posNum[Long], Gen.posNum[Int]) { (ins, value, h) =>
      val out = new ErgoBoxCandidate(
        value,
        ErgoScriptPredef.feeProposition(),
        h,
        Seq.empty[(ErgoBox.TokenId, Long)].toColl,
        Map.empty
      )
      val unsignedInputs = ins
        .map { box =>
          new UnsignedInput(box.id)
        }
        .toIndexedSeq
      val unsignedTx = new UnsignedErgoLikeTransaction(
        unsignedInputs,
        IndexedSeq(),
        IndexedSeq(out)
      )

      val signedTxFull = fullProver.sign(unsignedTx, ins.toIndexedSeq, IndexedSeq(), stateContext).get
      val signedTxUnsafe = unsafeProver.prove(unsignedTx, extendedSecretKey.key)

      signedTxFull shouldEqual signedTxUnsafe

      signedTxFull.inputs.map(_.spendingProof.proof).zip(signedTxFull.inputs.map(_.spendingProof.proof))
        .foreach { case (fullProof, unsafeProof) =>
          ErgoSignature.verify(unsignedTx.messageToSign, fullProof, extendedSecretKey.publicKey.key.h) shouldBe
            ErgoSignature.verify(unsignedTx.messageToSign, unsafeProof, extendedSecretKey.publicKey.key.h)
        }
    }
  }

  private val parameters = new ErgoLikeParameters {

    override def storageFeeFactor: Int = 1250000

    override def minValuePerByte: Int = 360

    override def maxBlockSize: Int = 524288

    override def tokenAccessCost: Int = 100

    override def inputCost: Int = 2000

    override def dataInputCost: Int = 100

    override def outputCost: Int = 100

    override def maxBlockCost: Long = 1000000

    override def softForkStartingHeight: Option[Int] = None

    override def softForkVotesCollected: Option[Int] = None

    override def blockVersion: Byte = 1
  }

  private val stateContext = new ErgoLikeStateContext {

    override def sigmaLastHeaders: Coll[Header] = Colls.emptyColl

    override def previousStateDigest: ADDigest = Base16.decode("a5df145d41ab15a01e0cd3ffbab046f0d029e5412293072ad0f5827428589b9302")
      .fold(_ => throw new Error(s"Failed to parse genesisStateDigest"), ADDigest @@ _)

    override def sigmaPreHeader: PreHeader = CPreHeader(
      version = 0,
      parentId = Colls.emptyColl[Byte],
      timestamp = 0,
      nBits = 0,
      height = 0,
      minerPk = CGroupElement(CryptoConstants.dlogGroup.generator),
      votes = Colls.emptyColl[Byte]
    )
  }

  private def propGen(dlog: ProveDlog): Gen[ErgoTree] = Gen.const(dlog.toSigmaProp)

}
