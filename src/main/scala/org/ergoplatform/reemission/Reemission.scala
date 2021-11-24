package org.ergoplatform.reemission

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.ErgoScriptPredef.{boxCreationHeight, expectedMinerOutScriptBytesVal}
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.{ErgoAddressEncoder, Height, MinerPubkey, Outputs, Self}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings}
import scorex.util.ModifierId
import sigmastate.{AND, EQ, GE, GT, Minus, OR}
import sigmastate.Values.{ErgoTree, IntConstant}
import sigmastate.utxo.{ByIndex, ExtractAmount, ExtractScriptBytes}


class Reemission(ms: MonetarySettings) {
  import Reemission._

  val emissionRules = new EmissionRules(ms)

  val basicChargeAmount = 21 // in ERG

  def reemissionForHeight(height: Height): Long = {
    val emission = emissionRules.emissionAtHeight(height)
    if (height >= ActivationHeight && emission >= (basicChargeAmount + 3) * EmissionRules.CoinsInOneErgo) {
      basicChargeAmount * EmissionRules.CoinsInOneErgo
    } else if (emission > 3 * EmissionRules.CoinsInOneErgo) {
      emission - 3 * EmissionRules.CoinsInOneErgo
    } else {
      0L
    }
  }

  val reemissionBoxProp: ErgoTree = {
    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    // output to pay miner
    val minerOut = ByIndex(Outputs, IntConstant(1))

    // miner's output must have script which is time-locking reward for miner's pubkey
    // box height must be the same as block height
    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), expectedMinerOutScriptBytesVal(ms.minerRewardDelay, MinerPubkey)),
      EQ(Height, boxCreationHeight(minerOut))
    )

    // reemission output's height must be the same as block height
    val heightCorrect = EQ(boxCreationHeight(reemissionOut), Height)

    // reemission output's height is greater than reemission input
    val heightIncreased = GT(Height, boxCreationHeight(Self))

    // check that height is greater than end of emission (>= 2,080,800 for the mainnet)
    val afterEmission = GE(Height, IntConstant(emissionPeriod))

    // reemission contract must be preserved
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(reemissionOut))

    // miner's reward
    val coinsToIssue = ms.oneEpochReduction // 3 ERG
    val correctCoinsIssued = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(reemissionOut)))

    val sponsored = GT(ExtractAmount(reemissionOut), ExtractAmount(Self))

    AND(
      sameScriptRule,
      heightCorrect,
      OR(
        AND(sponsored),
        AND(
          correctMinerOutput,
          afterEmission,
          heightIncreased,
          correctCoinsIssued
        )
      )
    ).toSigmaProp.treeWithSegregation
  }

}


object Reemission {

  val checkReemissionRules = true

  val EmissionNftId = ModifierId @@ ""

  val ReemissionTokenId = ModifierId @@ ""

  val ActivationHeight = 700000

  val emissionPeriod = 2080800

  def main(args: Array[String]): Unit = {
    val settings = ErgoSettings.read()
    val ms = settings.chainSettings.monetary
    println("Monetary settings: " + ms)
    val reemission = new Reemission(ms)
    val et = reemission.reemissionBoxProp
    val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
    println("p2s address: " + enc.fromProposition(et))

    var lowSet = false

    val total = (ActivationHeight to emissionPeriod).map { h =>
      val e = reemission.emissionRules.emissionAtHeight(h) / EmissionRules.CoinsInOneErgo
      val r = reemission.reemissionForHeight(h) / EmissionRules.CoinsInOneErgo

      if ((e - r) == 3 && !lowSet) {
        println("Start of low emission period: " + h)
        lowSet = true
      }
      if ((h % 65536 == 0) || h == ActivationHeight) {
        println(s"Emission at height $h : " + e)
        println(s"Reemission at height $h : " + r)
      }
      r
    }.sum

    val totalBlocks = total / 3 // 3 erg per block
    println("Total reemission: " + total + " ERG")
    println("Total reemission is enough for: " + totalBlocks + " blocks (" + totalBlocks / 720.0 / 365.0 + " years")
  }
}
