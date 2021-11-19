package org.ergoplatform.reemission

import org.ergoplatform.ErgoScriptPredef.{boxCreationHeight, expectedMinerOutScriptBytesVal}
import org.ergoplatform.{ErgoAddressEncoder, Height, MinerPubkey, Outputs, Self}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings}
import scorex.util.ModifierId
import sigmastate.{AND, EQ, GE, GT, Minus, OR}
import sigmastate.Values.{ErgoTree, IntConstant}
import sigmastate.utxo.{ByIndex, ExtractAmount, ExtractScriptBytes}

object Reemission {

  val checkReemissionRules = true

  val EmissionNftId = ModifierId @@ ""

  val ReemissionTokenId = ModifierId @@ ""

  val ActivationHeight = 0

  val emissionPeriod = 2080800

  def reemissionBoxProp(s: MonetarySettings): ErgoTree = {
    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    // output to pay miner
    val minerOut = ByIndex(Outputs, IntConstant(1))

    // miner's output must have script which is time-locking reward for miner's pubkey
    // box height must be the same as block height
    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), expectedMinerOutScriptBytesVal(s.minerRewardDelay, MinerPubkey)),
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
    val coinsToIssue = s.oneEpochReduction // 3 ERG
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

  def main(args: Array[String]): Unit = {
    val settings = ErgoSettings.read()
    val ms = settings.chainSettings.monetary
    println("Monetary settings: " + ms)
    val et = reemissionBoxProp(ms)
    val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
    println("p2s address: " + enc.fromProposition(et))

  }
}
