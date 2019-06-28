package org.ergoplatform.utils

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.{ErgoBox, Input, UnsignedInput}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettings, LaunchParameters, VotingSettings}
import org.ergoplatform.wallet.interpreter.{ErgoInterpreter, ErgoProvingInterpreter}
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.protocol.context.TransactionContext
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigmastate.interpreter.{ContextExtension, HintsBag, OtherCommitment}
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.lang.Terms._
import sigmastate.interpreter._
import scorex.util.bytesToId

object MultiSigner extends App {
  implicit lazy val ircontext: IRContext = new RuntimeIRContext

  implicit val vs = VotingSettings(64, 32, 128)

  val compiler = SigmaCompiler(TestnetNetworkPrefix, TransformingSigmaBuilder)

  val seedStrA = "edge talent poet tortoise trumpet dose"
  val seedStrB = "ergo talent poet tortoise trumpet rose"
  val seedStrC = "bitcoin talent poet tortoise trumpet rose"

  val secretsA = IndexedSeq(ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed(seedStrA)))
  val secretsB = IndexedSeq(ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed(seedStrB)))
  val secretsC = IndexedSeq(ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed(seedStrC)))

  val proverA = new ErgoProvingInterpreter(secretsA, LaunchParameters, HintsBag.empty)
  val proverB = new ErgoProvingInterpreter(secretsB, LaunchParameters, HintsBag.empty)
  val proverC = new ErgoProvingInterpreter(secretsC, LaunchParameters, HintsBag.empty)

  implicit val verifier = new ErgoInterpreter(LaunchParameters)

  val pubkeyA = proverA.pubKeyDlogs.head
  val pubkeyB = proverB.pubKeyDlogs.head
  val pubkeyC = proverC.pubKeyDlogs.head

  val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
  val prop = compiler.compile(env, """atLeast(2, Coll(pubkeyA, pubkeyB, pubkeyC))""").asSigmaProp

  val inputIndex = 0
  val boxToSpend: ErgoBox = ErgoBox(1000000000, prop, 0)
  val input = new UnsignedInput(boxToSpend.id, ContextExtension.empty)
  val undersignedTx = UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(boxToSpend.toCandidate))
  val transactionContext = TransactionContext(IndexedSeq(boxToSpend), IndexedSeq(), undersignedTx, selfIndex = 0)
  val genesisStateDigest = ADDigest @@ Array.fill(33)(0: Byte)

  val sol = AutolykosSolution(
    CryptoConstants.dlogGroup.generator,
    CryptoConstants.dlogGroup.generator,
    Array.fill(8)(0:Byte),
    0)

  val h = Header(1.toByte, bytesToId(Array.fill(32)(0: Byte)), Digest32 @@ Array.fill(32)(0: Byte),
    ADDigest @@ Array.fill(33)(0: Byte), Digest32 @@ Array.fill(32)(0: Byte), 0L, 0L, 0,
    Digest32 @@ Array.fill(32)(0: Byte), sol, Array.fill(3)(0: Byte))
  val stateContext = new ErgoStateContext(Seq(h), None, genesisStateDigest, LaunchParameters, ErgoValidationSettings.initial, VotingData.empty)

  val parameters = LaunchParameters
  val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty, parameters.maxBlockCost, 0)

  val msgToSign = undersignedTx.messageToSign

  val signedTx = proverA.generateCommitmentFor(pubkeyA).map{case  (aRandomness, aCommitment) =>
    val dlAKnown = OtherCommitment(pubkeyA, aCommitment)
    val hintsB = new HintsBag(Seq(dlAKnown))

    val proofB = proverB.prove(prop, context, msgToSign, hintsB).get

    val hintsA = proverA.bagForMultisig(context, prop, proofB.proof, Seq(pubkeyB, pubkeyC))
                      .addHint(OwnCommitment(pubkeyA, aRandomness, aCommitment))
    val proofA = proverA.prove(prop, context, msgToSign, hintsA).get

    val check = verifier.verify(prop, context, proofA, msgToSign)
    println(check)

    val input = Input(boxToSpend.id, proofA)
    val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(boxToSpend.toCandidate))

    println(tx.validateStateful(IndexedSeq(boxToSpend), IndexedSeq(), stateContext, 0).result.isValid)

    tx
  }
}