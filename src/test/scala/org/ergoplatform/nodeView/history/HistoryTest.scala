package org.ergoplatform.nodeView.history

import java.io.File

import io.circe
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ChainGenerator, ErgoGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.TestkitHelpers

class HistoryTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {
  val fullHistorySettings: ErgoSettings = new ErgoSettings {
    override def settingsJSON: Map[String, circe.Json] = Map()
    override val verifyTransactions: Boolean = true
    override val dataDir: String = s"/tmp/ergo/test-history"
  }
  new File(fullHistorySettings.dataDir).mkdirs()

  val BlocksInChain = 30

  //TODO write tests for other regimes
  var fullHistory = ErgoHistory.readOrGenerate(fullHistorySettings)

  assert(fullHistory.bestHeader.id sameElements fullHistory.bestFullBlockId.get)

  property("compare()") {
    //TODO what if headers1 > headers2 but fullchain 1 < fullchain2?
  }

  property("continuationIds()") {
    //TODO
  }

  property("syncInfo()") {
    /*
        val chain = genChain(BlocksInChain, Seq(history.bestFullBlock)).tail
        val answer = Random.nextBoolean()
        history = applyChain(history, chain)
        val si = history.syncInfo(answer)
        si.answer shouldBe answer
        si.lastBlockIds.flatten shouldEqual history.lastBlocks(Math.max(ErgoSyncInfo.MaxBlockIds, history.fullBlocksHeight)).flatMap(_.id)
    */
  }

  property("modifierById() should return correct type") {
    /*
        val chain = genChain(BlocksInChain, Seq(history.bestFullBlock)).tail
        chain.foreach { block =>
          history.modifierById(block.id).isDefined shouldBe false

          history = history.append(block.header).get._1
          history.headerById(block.id).isDefined shouldBe true
          history.fullBlockById(block.id).isDefined shouldBe false
          history.modifierById(block.id).isInstanceOf[Some[ErgoHeader]] shouldBe true

          history = history.append(block).get._1
          history.headerById(block.id).isDefined shouldBe true
          history.fullBlockById(block.id).isDefined shouldBe true
          history.modifierById(block.id).isInstanceOf[Some[ErgoFullBlock]] shouldBe true
        }
        */
  }

  property("heightOf() should return height of all blocks") {
    /*
        val chain = genChain(BlocksInChain, Seq(history.bestFullBlock)).tail
        chain.foreach { block =>
          val inHeight = history.fullBlocksHeight
          history = history.append(block.header).get._1.append(block).get._1
          history.heightOf(block).get shouldBe (inHeight + 1)
          history.headersHeight shouldBe (inHeight + 1)
          history.fullBlocksHeight shouldBe (inHeight + 1)
        }
        chain.foreach(block => history.heightOf(block).isDefined shouldBe true)
    */
  }

  property("lastBlocks() should return last blocks") {
    /*
        val blocksToApply = BlocksInChain
        val chain = genChain(blocksToApply, Seq(history.bestFullBlock)).tail
        history = applyChain(history, chain)
        history.fullBlocksHeight should be > blocksToApply
        val lastBlocks = history.lastBlocks(blocksToApply)
        lastBlocks.length shouldBe blocksToApply
        lastBlocks.foreach(b => assert(chain.contains(b)))
        lastBlocks.last shouldBe history.bestFullBlock
    */
  }

  property("Appended headers to best chain in history") {
    val chain = genHeaderChain(BlocksInChain, Seq(fullHistory.bestHeader)).tail
    chain.foreach { header =>
      fullHistory.contains(header) shouldBe false
      fullHistory.applicable(header) shouldBe true

      fullHistory = fullHistory.append(header).get._1

      fullHistory.contains(header) shouldBe true
      fullHistory.applicable(header) shouldBe false
      fullHistory.bestHeader shouldBe header
    }
  }

  property("Drop last block from history") {
    /*
        val chain = genChain(BlocksInChain, Seq(history.bestFullBlock)).tail
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
    */
  }

  property("process fork") {
    /*
        forAll(smallInt) { forkLength: Int =>
          whenever(forkLength > 0) {
            val fork1 = genChain(forkLength, Seq(history.bestFullBlock)).tail
            val fork2 = genChain(forkLength + 1, Seq(history.bestFullBlock)).tail

            history = applyChain(history, fork1)
            history.bestHeader shouldBe fork1.last.header
            history.bestFullBlock shouldBe fork1.last

            history = applyChain(history, fork2)
            history.bestHeader shouldBe fork2.last.header
            history.bestFullBlock shouldBe fork2.last
          }
        }
    */
  }


}
