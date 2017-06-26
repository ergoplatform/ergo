package org.ergoplatform.nodeView.history

import java.io.File

import io.circe
import org.ergoplatform.ErgoGenerators
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.block.ErgoHeader
import org.ergoplatform.settings.ErgoSettings
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.utils.NetworkTime
import scorex.testkit.TestkitHelpers

import scala.util.Random

class HistoryTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers {
  val settings: ErgoSettings = new ErgoSettings {
    override def settingsJSON: Map[String, circe.Json] = Map()

    override val dataDir: String = s"/tmp/ergo/${Random.alphanumeric.take(8)}"
  }
  new File(settings.dataDir).mkdirs()

  def genValidHeader(history: ErgoHistory): ErgoHeader =  {
    Miner.genBlock(BigInt(1),
      history.bestHeader,
      Array.fill(32)(0.toByte),
      Array.fill(32)(0.toByte),
      NetworkTime.time())
  }

  val history = ErgoHistory.readOrGenerate(settings)

  property("Appended block is in history") {
    var h = history
    check { _ =>
      val header = genValidHeader(h)
      h.modifierById(header.id).isDefined shouldBe false
      h = h.append(header).get._1
      h.modifierById(header.id).isDefined shouldBe true
    }
  }

}
