package org.ergoplatform.reemission

import org.ergoplatform.ErgoScriptPredef.boxCreationHeight
import org.ergoplatform.{Height, Outputs, Self}
import org.ergoplatform.settings.MonetarySettings
import sigmastate.{AND, EQ, GE, GT, Minus, OR}
import sigmastate.Values.{ErgoTree, IntConstant}
import sigmastate.utxo.{ByIndex, ExtractAmount}

object Reemission {
  def reemissionBoxProp(s: MonetarySettings): ErgoTree = {
    val rewardOut = ByIndex(Outputs, IntConstant(0))
    val heightCorrect = EQ(boxCreationHeight(rewardOut), Height)
    val heightIncreased = GT(Height, boxCreationHeight(Self))
    val validPeriod = GE(Height, IntConstant(2080800))

    val coinsToIssue = s.oneEpochReduction // 3 ERG
    val correctCoinsIssued = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(rewardOut)))

    val sponsored = GT(ExtractAmount(rewardOut), ExtractAmount(Self))

    OR(
      AND(
        sponsored,
        heightCorrect),
      AND(
        validPeriod,
        heightIncreased,
        heightCorrect,
        correctCoinsIssued
      )
    ).toSigmaProp.treeWithSegregation
  }
}
