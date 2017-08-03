package org.ergoplatform.nodeView.history

import java.io.File

import io.circe.Json
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.{ChainGenerator, ErgoGenerators}
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

  protected def generateHistory(verify: Boolean, adState: Boolean, toKeep: Int, nonce: Long = 0): ErgoHistory = {
    val paramsHash = Base58.encode(Algos.hash(verify.toString + adState + toKeep))
    val fullHistorySettings: ErgoSettings = new ErgoSettings {
      override def settingsJSON: Map[String, Json] = Map()

      override val verifyTransactions: Boolean = verify
      override val ADState: Boolean = adState
      override val dataDir: String = s"/tmp/ergo/test-history-$paramsHash-$nonce"
      override val blocksToKeep: Int = toKeep
    }
    new File(fullHistorySettings.dataDir).mkdirs()
    ErgoHistory.readOrGenerate(fullHistorySettings)
  }

}
