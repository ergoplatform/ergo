package org.ergoplatform.nodeView.history

import java.io.File

import io.circe.Json
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions.EmptyBlockTransactionsProcessor
import org.ergoplatform.settings.{Algos, ErgoSettings, NodeConfigurationSettings}
import org.ergoplatform.utils.{ChainGenerator, ErgoGenerators}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.settings.Settings
import scorex.crypto.encode.Base58
import scorex.testkit.TestkitHelpers

import scala.concurrent.duration._

trait HistorySpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {

  override lazy val smallInt: Gen[Int] = Gen.choose(0, BlocksInChain)

  val BlocksInChain = 10
  val BlocksToKeep = BlocksInChain + 1

  def bestFullOptToSeq(history: ErgoHistory): Seq[ErgoFullBlock] = history.bestFullBlockOpt.toSeq

  def ensureMinimalHeight(history: ErgoHistory, height: Int = BlocksInChain): ErgoHistory = {
    val historyHeight = history.height
    if (historyHeight < height) {
      history match {
        case _: EmptyBlockTransactionsProcessor =>
          val chain = genHeaderChain(height - historyHeight, history)
          if(history.isEmpty) applyHeaderChain(history, chain) else applyHeaderChain(history, chain.tail)
        case _ =>
          ???
      }
    } else {
      history
    }
  }

  def generateHistory(verify: Boolean,
                      adState: Boolean,
                      popow: Boolean,
                      blocksToKeep: Int,
                      nonce: Long = 0,
                      epochLength: Int = 100000000): ErgoHistory = {

    val blockInterval = 1.minute
    val minimalSuffix = 2
    val paramsHash = Base58.encode(Algos.hash(verify.toString + adState + blocksToKeep + popow))
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(adState, verify, blocksToKeep,
      popow, minimalSuffix, blockInterval, epochLength)
    val scorexSettings: Settings = new Settings {
      override def settingsJSON: Map[String, Json] = Map()
    }

    val fullHistorySettings: ErgoSettings = ErgoSettings(s"/tmp/ergo/test-history-$paramsHash-$nonce",
      nodeSettings,
      scorexSettings)
    new File(fullHistorySettings.directory).mkdirs()
    ErgoHistory.readOrGenerate(fullHistorySettings)
  }
}
