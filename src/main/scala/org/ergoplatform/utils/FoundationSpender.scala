package org.ergoplatform.utils

import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.{ErgoValidationSettings, LaunchParameters, VotingSettings}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.protocol.context.TransactionContext
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigmastate.interpreter.{ContextExtension, HintsBag}
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.lang.Terms._
import sigmastate.interpreter._
import scorex.util.bytesToId
import scorex.util.encode.Base16

/*
object FounderSpender extends App {
  implicit lazy val ircontext: IRContext = new RuntimeIRContext
  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)

  val seed = "..."
  val height = 1000

  val prover = new OldProvingInterpreter(seed, LaunchParameters, HintsBag.empty)
  implicit val verifier = new ErgoInterpreter(LaunchParameters)


  //forming a context. Only height matters for the foundation box.
  implicit val vs = VotingSettings(64, 32, 128)
  val parameters = LaunchParameters
  val genesisStateDigest = ADDigest @@ Array.fill(33)(0: Byte)
  val sol = AutolykosSolution(
    CryptoConstants.dlogGroup.generator,
    CryptoConstants.dlogGroup.generator,
    Array.fill(8)(0:Byte),
    0)
  val h = Header(1.toByte, bytesToId(Array.fill(32)(0: Byte)), Digest32 @@ Array.fill(32)(0: Byte),
    ADDigest @@ Array.fill(33)(0: Byte), Digest32 @@ Array.fill(32)(0: Byte), 0L, 0L, height,
    Digest32 @@ Array.fill(32)(0: Byte), sol, Array.fill(3)(0: Byte))
  val stateContext = new ErgoStateContext(Seq(h), None, genesisStateDigest, parameters, ErgoValidationSettings.initial, VotingData.empty)


  //box and message to sign
  val box = Base16.decode("80f2a7d2901d0008cd031fb52cf6e805f80d97cde289f4f757d49accf0c83fb864b27d2cf982c37f9a8be8070000826c570297778509ee3783cf7ef174e27e975aca5eb8150f39bf59f2b0eb018000").get
  val gfBox = ErgoBoxSerializer.parseBytes(box)

  val boxToSpend: ErgoBox = gfBox
  val input = new UnsignedInput(boxToSpend.id, ContextExtension.empty)

  //outputs
  val fee = EmissionRules.CoinsInOneErgo / 10 //0.1 Erg
  val feeBox = new ErgoBox(fee, ErgoScriptPredef.feeProposition(), height)

  val receiverKey = ergoAddressEncoder.fromString("9f3JCikU5866Yr5JGjfK9hJpSvBu3veHyYmpVbosJ3cTCyWDJhC").get.script
  val withdrawalAmount = 1000 * EmissionRules.CoinsInOneErgo
  val withdrawalOutputs = IndexedSeq(new ErgoBoxCandidate(withdrawalAmount, receiverKey, height))

  val changeOut = new ErgoBoxCandidate(gfBox.value - withdrawalAmount - fee, gfBox.ergoTree, height,
    gfBox.additionalTokens, gfBox.additionalRegisters)

  val outputCandidates = IndexedSeq(changeOut) ++ withdrawalOutputs ++ IndexedSeq(feeBox.toCandidate)
  val undersignedTx = UnsignedErgoTransaction(IndexedSeq(input), outputCandidates)
  val transactionContext = TransactionContext(IndexedSeq(boxToSpend), IndexedSeq(), undersignedTx, selfIndex = 0)
  val msgToSign = undersignedTx.messageToSign

  val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty, parameters.maxBlockCost, 0)
  val prop = gfBox.proposition.asSigmaProp


  val proof = prover.prove(prop, context, msgToSign, HintsBag.empty).get

  val check = verifier.verify(prop, context, proof, msgToSign)
  println("proof is correct: " + check)

  val tx = ErgoTransaction(IndexedSeq(Input(boxToSpend.id, proof)), undersignedTx.outputCandidates)

  println(ErgoTransaction.transactionEncoder.apply(tx))
}
*/
