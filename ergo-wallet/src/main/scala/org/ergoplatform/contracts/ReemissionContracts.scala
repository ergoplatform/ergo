package org.ergoplatform.contracts

import org.ergoplatform.ErgoBox.{R2, STokensRegType}
import org.ergoplatform.ErgoTreePredef.{boxCreationHeight, expectedMinerOutScriptBytesVal}
import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.settings.MonetarySettings
import sigmastate._
import sigma.Coll
import sigma.ast._
import sigma.ast.syntax._

/**
  * Container for re-emission related contracts. Contains re-emission contract and pay-to-reemission contract.
  */
trait ReemissionContracts {

  /**
    * How much miner can take per block from re-emission contract
    */
  val reemissionRewardPerBlock: Long = 3 * CoinsInOneErgo // 3 ERG

  /**
    * @return - ID of NFT token associated with re-emission contract
    */
  def reemissionNftIdBytes: Coll[Byte]

  /**
    * @return - height when reemission starts
    */
  def reemissionStartHeight: Int

  /** Helper method to extract tokens from a box. */
  private def extractTokens(box: Value[SBox.type]): OptionGet[SCollection[STuple]] = {
    val rOutTokens = OptionGet(ExtractRegisterAs(box, R2)(STokensRegType))
    rOutTokens
  }

  /** Helper method to produce v1 tree from a SigmaPropValue instance (i.e. root node of AST).*/
  private def v1Tree(prop: SigmaPropValue): ErgoTree = {
    val version: Byte = 1
    // it used to be ErgoTree.headerWithVersion(version) before Sigma v5.0.14,
    // which used the default header, now this is made explicit via naming
    val headerFlags = ErgoTree.defaultHeaderWithVersion(version)
    ErgoTree.fromProposition(headerFlags, prop)
  }

  /**
    * Contract for boxes miners paying to remission contract according to EIP-27.
    * Anyone can merge multiple boxes locked by this contract with reemission box
    */
  lazy val payToReemission: ErgoTree = v1Tree({
    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))
    val rOutTokens = extractTokens(reemissionOut)

    val firstTokenId = SelectField(ByIndex(rOutTokens, IntConstant(0)), 1.toByte)

    EQ(firstTokenId, ByteArrayConstant(reemissionNftIdBytes))
  })

  /**
    * Re-emission contract
    */
  def reemissionBoxProp(ms: MonetarySettings): ErgoTree = v1Tree({

    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    val secondOut = ByIndex(Outputs, IntConstant(1))

    // output to pay miner
    val minerOut = secondOut

    // check that first (re-emission) output contains re-emission NFT (in the first position)
    val rOutTokens = extractTokens(reemissionOut)
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
    val merging = {
      val feeOut = secondOut
      AND(
        GT(ExtractAmount(reemissionOut), ExtractAmount(Self)),
        LE(ExtractAmount(feeOut), LongConstant(CoinsInOneErgo / 100)), // 0.01 ERG
        EQ(SizeOf(Outputs), 2)
      )
    }

    AND(
      correctNftId,
      sameScriptRule,
      OR(
        merging,
        AND(
          heightCorrect,
          correctMinerOutput,
          afterEmission,
          heightIncreased,
          correctCoinsIssued
        )
      )
    )
  }.toSigmaProp)

}
