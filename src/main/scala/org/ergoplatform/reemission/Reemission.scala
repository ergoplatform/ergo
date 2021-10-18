package org.ergoplatform.reemission

import org.ergoplatform.ErgoScriptPredef.{boxCreationHeight, expectedMinerOutScriptBytesVal}
import org.ergoplatform.{Height, MinerPubkey, Outputs, Self}
import org.ergoplatform.settings.MonetarySettings
import sigmastate.{AND, EQ, GE, GT, Minus, OR}
import sigmastate.Values.{ErgoTree, IntConstant}
import sigmastate.utxo.{ByIndex, ExtractAmount, ExtractScriptBytes}

object Reemission {

  val emissionPeriod = 2080800

  def reemissionBoxProp(s: MonetarySettings): ErgoTree = {
    val reemissionOut = ByIndex(Outputs, IntConstant(0))
    val minerOut = ByIndex(Outputs, IntConstant(1))

    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), expectedMinerOutScriptBytesVal(s.minerRewardDelay, MinerPubkey)),
      EQ(Height, boxCreationHeight(minerOut))
    )

    val heightCorrect = EQ(boxCreationHeight(reemissionOut), Height)
    val heightIncreased = GT(Height, boxCreationHeight(Self))
    val afterEmission = GE(Height, IntConstant(emissionPeriod))

    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
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
}
