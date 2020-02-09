package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.wallet.ErgoWalletActor.WalletVars
import org.ergoplatform.settings.LaunchParameters
import org.ergoplatform.utils.{ErgoPropertyTest, WalletTestOps}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.scalatest.PropSpec
import WalletScanLogic.{extractWalletOutputs, scanBlockTransactions}
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.boxes.BoxCertainty
import org.scalacheck.Gen
import sigmastate.Values.{ErgoTree, FalseLeaf}

import scala.util.Random

class WalletScanLogicSpec extends ErgoPropertyTest with WalletTestOps {

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)
  private val prover = defaultProver
  private val monetarySettings = initSettings.chainSettings.monetary.copy(minerRewardDelay = 720)
  private val s = initSettings.copy(chainSettings = initSettings.chainSettings.copy(monetary = monetarySettings))
  private val walletVars = WalletVars(Some(prover), Seq.empty)(settings = s)

  property("extractWalletOutputs properly extracts") {
    val height = Random.nextInt(200) - 100
    val inclusionHeightOpt = if (height <= 0) None else Some(height)

    val pubkeys = walletVars.trackedPubKeys
    val miningScripts = walletVars.miningScripts

    val paymentsGen: Gen[List[ErgoTree]] = Gen.listOf(Gen.oneOf(pubkeys.map(_.toSigmaProp: ErgoTree)))
    val miningRewardsGen: Gen[List[ErgoTree]] = Gen.listOf(Gen.oneOf(miningScripts))
    val nonTrackablePaymentsGen: Gen[List[ErgoTree]] = Gen.listOf(Gen.const(FalseLeaf.toSigmaProp))

    forAll(paymentsGen, miningRewardsGen, nonTrackablePaymentsGen) { (payments, miningRewards, nonTrackablePayments) =>
      val outputsNum = payments.length + miningRewards.length + nonTrackablePayments.length

      def valueGen(): Int = Random.nextInt(1000) + 100

      whenever(outputsNum > 0) {
        val paymentValues = payments.map(_ => valueGen())
        val miningRewardValues = miningRewards.map(_ => valueGen())
        val nonTrackableValues = nonTrackablePayments.map(_ => valueGen())

        val outs = (payments ++ miningRewards ++ nonTrackablePayments)
          .zip(paymentValues ++ miningRewardValues ++ nonTrackableValues)
          .map { case (script, value) => new ErgoBoxCandidate(value, script, creationHeight = 1) }
          .toIndexedSeq

        val tx = new ErgoTransaction(fakeInputs, IndexedSeq.empty, outs)

        val foundBoxes = extractWalletOutputs(tx, inclusionHeightOpt, walletVars)
        foundBoxes.length shouldBe (payments.length + miningRewards.length)
        foundBoxes.map(_.inclusionHeightOpt).forall(_ == inclusionHeightOpt) shouldBe true
        foundBoxes.map(_.value).sum shouldBe (paymentValues ++ miningRewardValues).sum
        foundBoxes.forall(tb => if(payments.contains(tb.box.ergoTree)){
          tb.certain(Constants.PaymentsAppId).get == BoxCertainty.Certain &&
            tb.applicationStatuses == Map(Constants.PaymentsAppId -> BoxCertainty.Certain)
        } else {
          tb.certain(Constants.MiningRewardsAppId).get == BoxCertainty.Certain &&
            tb.applicationStatuses == Map(Constants.MiningRewardsAppId -> BoxCertainty.Certain)
        }) shouldBe true
      }
    }
  }

  property("scanBlockTransactions") {

  }

}