package org.ergoplatform.nodeView.history

import java.io.File

import io.circe
import org.ergoplatform.ErgoGenerators
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.block.{ErgoFullBlock, ErgoHeader}
import org.ergoplatform.settings.ErgoSettings
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.utils.NetworkTime
import scorex.testkit.TestkitHelpers

import scala.annotation.tailrec
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


  @tailrec
  final def genChain(height: Int, acc: Seq[ErgoFullBlock]): Seq[ErgoFullBlock] = if (height == 0) {
    acc.reverse
  } else {
    val block = Miner.genBlock(BigInt(1), acc.head.header, Array.fill(32)(0.toByte), Seq(), NetworkTime.time())
    genChain(height - 1,  block +: acc)
  }

  val history = ErgoHistory.readOrGenerate(settings)

  property("Appended headers and blocks to best chain in history") {
    var h = history
    val chain = genChain(100, Seq(h.bestFullBlock)).tail
    chain.foreach { block =>
      val header = block.header
      val inBestBlock = h.bestFullBlock

      h.contains(header) shouldBe false
      h.contains(block) shouldBe false
      h.applicable(header) shouldBe true
      h.applicable(block) shouldBe false
      h.bestHeader shouldBe inBestBlock.header
      h.bestFullBlock shouldBe inBestBlock

      h = h.append(header).get._1

      h.contains(header) shouldBe true
      h.contains(block) shouldBe false
      h.applicable(header) shouldBe false
      h.applicable(block) shouldBe true
      h.bestHeader shouldBe header
      h.bestFullBlock shouldBe inBestBlock

      h = h.append(block).get._1

      h.contains(header) shouldBe true
      h.contains(block) shouldBe true
      h.applicable(header) shouldBe false
      h.applicable(block) shouldBe false
      h.bestHeader shouldBe header
      h.bestFullBlock shouldBe block
    }
  }

}
