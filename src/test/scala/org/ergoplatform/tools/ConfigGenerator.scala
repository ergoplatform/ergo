package org.ergoplatform.tools

import java.io.{File, PrintWriter}

import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.wallet.mnemonic.Mnemonic

object ConfigGenerator extends App with ErgoTestHelpers {

  val seedStrength = 128

  val qty = args(0).toInt
  val apiKeyHash = args(1)
  val digestMode = if (args(2) == "true") true else false
  val mining = if (args(3) == "true") !digestMode else false
  val saveDirPath = if (args.length < 5) "/tmp/ergo/configs" else args(4)

  (0 to qty).foreach { i =>
    val stateType = if (digestMode) "digest" else "utxo"
    val cfgName = s"node-$stateType-$i-local.conf"
    val nodeName = s"node-$stateType-$i-devnet"
    val mnemonic = genMnemonic
    dumpToFile(saveDirPath, cfgName, template(digestMode, mining, mnemonic, apiKeyHash, nodeName))
  }

  sys.exit(0)

  private def dumpToFile(dir: String, cfgName: String, content: String): Unit = {
    new File(dir).mkdirs()
    val outWriter = new PrintWriter(new File(s"$dir/$cfgName"))
    outWriter.write(content)
    outWriter.close()
  }

  private def template(digestMode: Boolean, mining: Boolean,
                       mnemonic: String, apiKeyHash: String, nodeName: String): String =
    s"""
       |ergo.node.stateType = "${if (digestMode) "digest" else "utxo"}"
       |ergo.node.mining = ${if (mining) "true" else "false"}
       |ergo.node.offlineGeneration = true
       |ergo.node.useExternalMiner = false
       |ergo.wallet.testMnemonic = "$mnemonic"
       |ergo.wallet.testKeysQty = 10
       |scorex.restApi.apiKeyHash = "$apiKeyHash"
       |scorex.network.nodeName = "$nodeName"
     """.stripMargin

  private def genMnemonic = {
    val entropy = scorex.utils.Random.randomBytes(seedStrength / 8)
    new Mnemonic(settings.walletSettings.mnemonicPhraseLanguage, settings.walletSettings.seedStrengthBits)
      .toMnemonic(entropy)
      .get
  }

}