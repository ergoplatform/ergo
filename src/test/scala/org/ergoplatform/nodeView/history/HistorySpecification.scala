package org.ergoplatform.nodeView.history

import java.io.File

import io.circe.Json
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions.EmptyBlockTransactionsProcessor
import org.ergoplatform.settings.{Algos, ErgoSettingsT}
import org.ergoplatform.utils.{ChainGenerator, ErgoGenerators}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.testkit.TestkitHelpers

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

  def bestFullOrGenesis(history: ErgoHistory): ErgoFullBlock = history.bestFullBlockOpt.getOrElse(ErgoFullBlock.genesis)

  def ensureMinimalHeight(history: ErgoHistory, height: Int = BlocksInChain): ErgoHistory = {
    val historyHeight = history.height
    if (historyHeight < height) {
      history match {
        case h: EmptyBlockTransactionsProcessor =>
          applyHeaderChain(history, genHeaderChain(height - historyHeight, Seq(history.bestHeader)).tail)
        case h =>
          ???
      }

    } else {
      history
    }
  }

  def generateHistory(verify: Boolean,
                      adState: Boolean,
                      popow: Boolean,
                      toKeep: Int,
                      nonce: Long = 0,
                      epoch: Int = 100000000): ErgoHistory = {
    val paramsHash = Base58.encode(Algos.hash(verify.toString + adState + toKeep + popow))
    val fullHistorySettings: ErgoSettingsT = new ErgoSettingsT {
      override def settingsJSON: Map[String, Json] = Map()

      override val epochLength: Int = epoch
      override val verifyTransactions: Boolean = verify
      override val ADState: Boolean = adState
      override lazy val dataDir: String = s"/tmp/ergo/test-history-$paramsHash-$nonce"
      override val blocksToKeep: Int = toKeep
      override val poPoWBootstrap: Boolean = popow
    }
    new File(fullHistorySettings.dataDir).mkdirs()
    ErgoHistory.readOrGenerate(fullHistorySettings)
  }

}
