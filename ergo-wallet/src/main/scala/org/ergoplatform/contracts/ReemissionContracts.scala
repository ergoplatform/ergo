package org.ergoplatform.contracts

import org.ergoplatform.ErgoBox.R2
import org.ergoplatform.ErgoScriptPredef.{boxCreationHeight, expectedMinerOutScriptBytesVal}
import org.ergoplatform.{Height, MinerPubkey, Outputs, Self}
import org.ergoplatform.settings.MonetarySettings
import sigmastate.{AND, EQ, GE, GT, LE, Minus, OR, SByte, SCollection, SLong, STuple}
import sigmastate.Values.{ByteArrayConstant, ErgoTree, IntConstant, LongConstant}
import sigmastate.utxo.{ByIndex, ExtractAmount, ExtractRegisterAs, ExtractScriptBytes, OptionGet, SelectField, SizeOf}

/**
  * Container for re-emission related contracts. Contains re-emission contract and pay-to-reemission contract.
  */
trait ReemissionContracts {

  /**
    * How much miner can take per block from re-emission contract
    */
  val reemissionRewardPerBlock: Long = 3 * 1000000000L // 3 ERG

  /**
    * @return - ID of NFT token associated with re-emission contract
    */
  def reemissionNftIdBytes: Array[Byte]

  /**
    * @return - height when reemission starts
    */
  def reemissionStartHeight: Int

  /**
    * Contract for boxes miners paying to remission contract according to EIP-27.
    * Anyone can merge multiple boxes locked by this contract with reemission box
    */
  lazy val payToReemission: ErgoTree = {
    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    val rOutTokens = OptionGet(ExtractRegisterAs(reemissionOut, R2)(SCollection(STuple(SCollection(SByte), SLong))))

    val firstTokenId = SelectField(ByIndex(rOutTokens, IntConstant(0)), 1.toByte)

    EQ(firstTokenId, ByteArrayConstant(reemissionNftIdBytes))
  }.toSigmaProp.treeWithSegregation

  /**
    * Re-emission contract
    */
  def reemissionBoxProp(ms: MonetarySettings): ErgoTree = {

    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    // output to pay miner
    val minerOut = ByIndex(Outputs, IntConstant(1))

    val rOutTokens = OptionGet(ExtractRegisterAs(reemissionOut, R2)(SCollection(STuple(SCollection(SByte), SLong))))

    val firstTokenId = SelectField(ByIndex(rOutTokens, IntConstant(0)), 1.toByte)

    val correctNftId = EQ(firstTokenId, ByteArrayConstant(reemissionNftIdBytes))

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
    val afterEmission = GE(Height, IntConstant(reemissionStartHeight))

    // reemission contract must be preserved
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(reemissionOut))

    // miner's reward condition
    val correctCoinsIssued = EQ(reemissionRewardPerBlock, Minus(ExtractAmount(Self), ExtractAmount(reemissionOut)))

    // when reemission contract box got merged with other boxes
    val sponsored = {
      val feeOut = ByIndex(Outputs, IntConstant(1))
      AND(
        GT(ExtractAmount(reemissionOut), ExtractAmount(Self)),
        LE(ExtractAmount(feeOut), LongConstant(10000000)), // 0.01 ERG
        EQ(SizeOf(Outputs), 2)
      )
    }

    AND(
      correctNftId,
      sameScriptRule,
      heightCorrect,
      OR(
        sponsored,
        AND(
          correctMinerOutput,
          afterEmission,
          heightIncreased,
          correctCoinsIssued
        )
      )
    )
  }.toSigmaProp.treeWithSegregation
}
