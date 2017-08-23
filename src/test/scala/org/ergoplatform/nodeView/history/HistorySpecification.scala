package org.ergoplatform.nodeView.history

import java.io.File

import io.circe.Json
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions.EmptyBlockTransactionsProcessor
import org.ergoplatform.settings.{Algos, ErgoSettings}
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
                      toKeep: Int,
                      nonce: Long = 0,
                      epoch: Int = 100000000): ErgoHistory = {
    val paramsHash = Base58.encode(Algos.hash(verify.toString + adState + toKeep + popow))
    val fullHistorySettings: ErgoSettings = new ErgoSettings {
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
