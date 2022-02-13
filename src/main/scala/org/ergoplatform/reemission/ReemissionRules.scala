package org.ergoplatform.reemission

import org.ergoplatform.ErgoBox.R2
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.ErgoScriptPredef.{boxCreationHeight, expectedMinerOutScriptBytesVal}
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.{ErgoAddressEncoder, Height, MinerPubkey, Outputs, Pay2SAddress, Self}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings, ReemissionSettings}
import sigmastate.{AND, EQ, GE, GT, LE, Minus, OR, SBoolean, SByte, SCollection, SLong, STuple}
import sigmastate.Values.{ByteArrayConstant, ErgoTree, IntConstant, LongConstant, Value}
import sigmastate.eval.CompiletimeIRContext
import sigmastate.lang.{CompilerSettings, SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.utxo.{ByIndex, ExtractAmount, ExtractRegisterAs, ExtractScriptBytes, OptionGet, SelectField, SizeOf}

import scala.util.Try


class ReemissionRules(reemissionSettings: ReemissionSettings) {

  val basicChargeAmount = 12 // in ERG

  /**
    * Contract for boxes miners paying to remission contract according to EIP-27.
    * Anyone can merge multiple boxes locked by this contract with reemission box
    */
  def payToReemission(reemissionNftId: Array[Byte]): ErgoTree = {
    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    val rOutTokens = OptionGet(ExtractRegisterAs(reemissionOut, R2)(SCollection(STuple(SCollection(SByte), SLong))))

    val firstTokenId = SelectField(ByIndex(rOutTokens, IntConstant(0)), 0.toByte)

    EQ(firstTokenId, ByteArrayConstant(reemissionNftId))
  }.toSigmaProp.treeWithSegregation

  /**
    * Reemission box contract
    */
  def reemissionBoxProp(ms: MonetarySettings): ErgoTree = {
    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    // output to pay miner
    val minerOut = ByIndex(Outputs, IntConstant(1))

    val rOutTokens = OptionGet(ExtractRegisterAs(reemissionOut, R2)(SCollection(STuple(SCollection(SByte), SLong))))

    val firstTokenId = SelectField(ByIndex(rOutTokens, IntConstant(0)), 1.toByte)

    val correctNftId = EQ(firstTokenId, ByteArrayConstant(reemissionSettings.reemissionNftIdBytes))

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
    val afterEmission = GE(Height, IntConstant(reemissionSettings.reemissionStartHeight))

    // reemission contract must be preserved
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(reemissionOut))

    // miner's reward
    val coinsToIssue = 3 * 1000000000L // 3 ERG
    val correctCoinsIssued = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(reemissionOut)))

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

  def reemissionForHeight(height: Height,
                          emissionRules: EmissionRules): Long = {
    val emission = emissionRules.emissionAtHeight(height)
    if (height >= reemissionSettings.activationHeight &&
          emission >= (basicChargeAmount + 3) * EmissionRules.CoinsInOneErgo) {
      basicChargeAmount * EmissionRules.CoinsInOneErgo
    } else if (emission > 3 * EmissionRules.CoinsInOneErgo) {
      emission - 3 * EmissionRules.CoinsInOneErgo
    } else {
      0L
    }
  }

  def injectionBoxP2SAddress(mainnet: Boolean): Pay2SAddress = {
    val networkPrefix = if (mainnet) {
      ErgoAddressEncoder.MainnetNetworkPrefix
    } else {
      ErgoAddressEncoder.TestnetNetworkPrefix
    }

    implicit val addrEncoder = new ErgoAddressEncoder(networkPrefix)

    val source =
      """
        |  {
        |    INPUTS(0).value > 15000000L * 1000000000L
        |  }
      """.stripMargin

    val compiler = SigmaCompiler(CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true))
    val compiled = Try(compiler.compile(Map.empty, source)(new CompiletimeIRContext)).get.asInstanceOf[Value[SBoolean.type]].toSigmaProp

    Pay2SAddress.apply(compiled)
  }

  def main(args: Array[String]): Unit = {
    val p2sAddress = injectionBoxP2SAddress(mainnet = false)
    println(new ErgoAddressEncoder(ErgoAddressEncoder.TestnetNetworkPrefix).fromString(p2sAddress.toString()))
    println("injectioon box p2s: " + p2sAddress)

    System.exit(10)

    val settings = ErgoSettings.read()

    println("Monetary settings: " + settings.chainSettings.monetary)
    println("Reemission settings: " + settings.chainSettings.reemission)
    val ms = settings.chainSettings.monetary
    val rs = settings.chainSettings.reemission
    val emissionRules = settings.chainSettings.emissionRules

    val et = reemissionBoxProp(ms)
    val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
    println("p2s address: " + enc.fromProposition(et))

    // reemission.tokenPreservation(Array.fill(32)(0: Byte), 0: Byte)

    var lowSet = false

    val total = (rs.activationHeight to rs.reemissionStartHeight).map { h =>
      val e = emissionRules.emissionAtHeight(h) / EmissionRules.CoinsInOneErgo
      val r = reemissionForHeight(h, emissionRules) / EmissionRules.CoinsInOneErgo

      if ((e - r) == 3 && !lowSet) {
        println("Start of low emission period: " + h)
        lowSet = true
      }
      if ((h % 65536 == 0) || h == rs.activationHeight) {
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
