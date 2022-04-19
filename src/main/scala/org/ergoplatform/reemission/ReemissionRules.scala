package org.ergoplatform.reemission

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.contracts.ReemissionContracts
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.{ErgoAddressEncoder, Pay2SAddress}
import org.ergoplatform.settings.{ErgoSettings, ReemissionSettings}
import sigmastate.SBoolean
import sigmastate.Values.Value
import sigmastate.eval.CompiletimeIRContext
import sigmastate.lang.{CompilerSettings, SigmaCompiler, TransformingSigmaBuilder}

import scala.util.Try

class ReemissionRules(reemissionSettings: ReemissionSettings) extends ReemissionContracts {

  override val reemissionNftIdBytes: Array[Byte] = reemissionSettings.reemissionNftIdBytes
  override val reemissionStartHeight: Height = reemissionSettings.reemissionStartHeight

  val basicChargeAmount = 12 // in ERG

  def reemissionForHeight(height: Height,
                          emissionRules: EmissionRules): Long = {
    val emission = emissionRules.emissionAtHeight(height)
    if (height >= reemissionSettings.activationHeight &&
      emission >= (basicChargeAmount + 3) * EmissionRules.CoinsInOneErgo) {
      basicChargeAmount * EmissionRules.CoinsInOneErgo
    } else if (height >= reemissionSettings.activationHeight &&
      emission > 3 * EmissionRules.CoinsInOneErgo) {
      emission - 3 * EmissionRules.CoinsInOneErgo
    } else {
      0L
    }
  }
}

object ReemissionRules {

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
        |    INPUTS(0).value > 30000000L * 1000000000L
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

    val settings = ErgoSettings.read()

    println("Monetary settings: " + settings.chainSettings.monetary)
    println("Reemission settings: " + settings.chainSettings.reemission)
    val ms = settings.chainSettings.monetary
    val rs = settings.chainSettings.reemission
    val emissionRules = settings.chainSettings.emissionRules
    val reemissionRules = settings.chainSettings.reemission.reemissionRules

    val et = reemissionRules.reemissionBoxProp(ms)
    val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
    println("p2s address: " + enc.fromProposition(et))

    // reemission.tokenPreservation(Array.fill(32)(0: Byte), 0: Byte)

    var lowSet = false

    val total = (777216 to rs.reemissionStartHeight).map { h =>
      val e = emissionRules.emissionAtHeight(h) / EmissionRules.CoinsInOneErgo
      val r = reemissionRules.reemissionForHeight(h, emissionRules) / EmissionRules.CoinsInOneErgo

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
