package org.ergoplatform.nodeView.history

import java.io.File

import io.circe
import org.ergoplatform.ErgoGenerators
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.block.ErgoFullBlock
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
    genChain(height - 1, block +: acc)
  }

  var history = ErgoHistory.readOrGenerate(settings)

  property("Appended headers and blocks to best chain in history") {
    val chain = genChain(100, Seq(history.bestFullBlock)).tail
    chain.foreach { block =>
      val header = block.header
      val inBestBlock = history.bestFullBlock

      history.contains(header) shouldBe false
      history.contains(block) shouldBe false
      history.applicable(header) shouldBe true
      history.applicable(block) shouldBe false
      history.bestHeader shouldBe inBestBlock.header
      history.bestFullBlock shouldBe inBestBlock

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.contains(block) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(block) shouldBe true
      history.bestHeader shouldBe header
      history.bestFullBlock shouldBe inBestBlock

      history = history.append(block).get._1

      history.contains(header) shouldBe true
      history.contains(block) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(block) shouldBe false
      history.bestHeader shouldBe header
      history.bestFullBlock shouldBe block
    }
  }

  property("Drop last block from history") {
    val chain = genChain(100, Seq(history.bestFullBlock)).tail
    chain.foreach { block =>
      val header = block.header
      val inBestBlock = history.bestFullBlock

      history.bestHeader shouldBe inBestBlock.header
      history.bestFullBlock shouldBe inBestBlock

      history = history.append(header).get._1.append(block).get._1

      history.bestHeader shouldBe header
      history.bestFullBlock shouldBe block

      history = history.drop(header.id)

      history.bestHeader shouldBe inBestBlock.header
      history.bestFullBlock shouldBe inBestBlock

      history = history.append(header).get._1.append(block).get._1

    }
  }

  property("process fork") {
    forAll(smallInt){ forkLength: Int  =>
      whenever(forkLength > 0) {
        val fork1 = genChain(forkLength, Seq(history.bestFullBlock)).tail
        val fork2 = genChain(forkLength + 1, Seq(history.bestFullBlock)).tail

        fork1.foreach { block =>
          history = history.append(block.header).get._1.append(block).get._1
        }
        history.bestHeader shouldBe fork1.last.header
        history.bestFullBlock shouldBe fork1.last

        fork2.foreach { block =>
          history = history.append(block.header).get._1.append(block).get._1
        }
        history.bestHeader shouldBe fork2.last.header
        history.bestFullBlock shouldBe fork2.last
      }
    }
  }


}
