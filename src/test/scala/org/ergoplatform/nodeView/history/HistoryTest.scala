package org.ergoplatform.nodeView.history

import java.io.File

import io.circe.Json
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.{ChainGenerator, ErgoGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.testkit.TestkitHelpers

import scala.util.Random

class HistoryTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {

  val BlocksToKeep = 21

  var fullHistory = generateHistory(verify = true, adState = true, BlocksToKeep)
  var txHistory = generateHistory(verify = true, adState = false, BlocksToKeep)
  var lightHistory = generateHistory(verify = false, adState = true, 0)
  assert(fullHistory.bestFullBlockIdOpt.isDefined)
  assert(lightHistory.bestFullBlockIdOpt.isEmpty)


  val BlocksInChain = 30

  property("syncInfo()") {
    val chain = genChain(BlocksInChain, Seq(fullHistory.bestFullBlockOpt.get)).tail
    val answer = Random.nextBoolean()
    fullHistory = applyChain(fullHistory, chain)
    val si = fullHistory.syncInfo(answer)
    si.answer shouldBe answer
    si.lastHeaderIds.last shouldEqual chain.last.header.id
    si.fullBlockIdOpt.get shouldEqual fullHistory.bestFullBlockIdOpt.get
  }

  property("Report invalid for best full block") {
    assert(fullHistory.bestFullBlockOpt.get.header == fullHistory.bestHeader)
    val chain = genChain(BlocksInChain, Seq(fullHistory.bestFullBlockOpt.get)).tail
    fullHistory = applyChain(fullHistory, chain)

    chain.takeRight(BlocksToKeep - 2).reverse.foreach { fullBlock =>
      fullHistory.bestFullBlockOpt.get.header shouldBe fullHistory.bestHeader
      fullHistory.bestHeader shouldEqual fullBlock.header

      val parentHeader = fullHistory.typedModifierById[Header](fullBlock.header.parentId).get
      fullHistory.contains(parentHeader.transactionsId) shouldBe true
      fullHistory.contains(parentHeader.ADProofsId) shouldBe true

      fullHistory = fullHistory.reportInvalid(fullBlock.blockTransactions)
      fullHistory.bestFullBlockOpt.get.header shouldBe fullHistory.bestHeader
      fullHistory.bestHeader shouldBe parentHeader
    }
  }

  property("continuationIds()") {
    var history = lightHistory
    val chain = genHeaderChain(100, Seq(history.bestHeader)).tail

    history = applyHeaderChain(history, chain)
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 1) {
        val si = ErgoSyncInfo(answer = true, Seq(chain.headers(chain.size - forkLength).id), None)
        val continuation = history.continuationIds(si, forkLength).get
        continuation.length shouldBe forkLength
        continuation.last._2 shouldEqual chain.last.id
        continuation.head._2 shouldEqual chain.headers(chain.size - forkLength).id
      }
    }
  }

  property("prune old blocks test") {
    var history = fullHistory
    val blocksToPrune = 10
    val chain = genChain(BlocksToKeep + blocksToPrune, Seq(history.bestFullBlockOpt.get)).tail

    history = applyChain(history, chain)
    history.bestHeader shouldBe chain.last.header
    history.bestFullBlockOpt.get.header shouldBe chain.last.header

    chain.take(blocksToPrune).foreach { b =>
      history.modifierById(b.header.transactionsId) shouldBe None
      history.modifierById(b.header.ADProofsId) shouldBe None
    }
    chain.takeRight(BlocksToKeep).foreach { b =>
      history.modifierById(b.header.transactionsId).isDefined shouldBe true
      history.modifierById(b.header.ADProofsId).isDefined shouldBe true
    }
  }

  property("commonBlockThenSuffixes()") {
    var history = lightHistory
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 10) {

        val fork1 = genHeaderChain(forkLength, Seq(history.bestHeader)).tail
        val common = fork1.headers(10)
        val fork2 = fork1.take(10) ++ genHeaderChain(forkLength + 1, Seq(common))

        history = applyHeaderChain(history, fork1)
        history.bestHeader shouldBe fork1.last

        val (our, their) = history.commonBlockThenSuffixes(fork2, history.bestHeader)
        our.head shouldBe their.head
        our.head shouldBe common
        our.last shouldBe fork1.last
        their.last shouldBe fork2.last
      }
    }
  }

  property("process fork") {
    var history = fullHistory
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 0) {
        val fork1 = genChain(forkLength, Seq(history.bestFullBlockOpt.get)).tail
        val fork2 = genChain(forkLength + 1, Seq(history.bestFullBlockOpt.get)).tail

        history = applyChain(history, fork1)
        history.bestHeader shouldBe fork1.last.header

        history = applyChain(history, fork2)
        history.bestHeader shouldBe fork2.last.header
      }
    }
  }

  property("Append headers to best chain in history") {
    var history = lightHistory
    val chain = genHeaderChain(BlocksInChain, Seq(history.bestHeader)).tail
    chain.headers.foreach { header =>
      val inHeight = history.heightOf(header.parentId).get
      history.contains(header) shouldBe false
      history.applicable(header) shouldBe true

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.applicable(header) shouldBe false
      history.bestHeader shouldBe header
      history.openSurfaceIds() shouldEqual Seq(header.id)
      history.heightOf(header.id).get shouldBe (inHeight + 1)
    }
  }

  property("Appended headers and transactions blocks to best chain in tx history") {
    var history = txHistory
    assert(history.bestFullBlockOpt.get.header == history.bestHeader)
    val chain = genChain(BlocksInChain, Seq(history.bestFullBlockOpt.get)).tail
    chain.foreach { fullBlock =>
      val startFullBlock = history.bestFullBlockOpt.get
      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      history.contains(header) shouldBe false
      history.contains(txs) shouldBe false
      history.applicable(header) shouldBe true
      history.applicable(txs) shouldBe false

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe true
      history.bestHeader shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock
      history.openSurfaceIds().head shouldEqual startFullBlock.header.id

      history = history.append(txs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe false
      history.bestHeader shouldBe header
      history.bestFullBlockOpt.get.header shouldBe fullBlock.header
    }
  }

  property("Appended full blocks to best chain in full history") {
    assert(fullHistory.bestFullBlockOpt.get.header == fullHistory.bestHeader)
    val chain = genChain(BlocksInChain, Seq(fullHistory.bestFullBlockOpt.get)).tail
    chain.foreach { fullBlock =>
      val startFullBlock = fullHistory.bestFullBlockOpt.get
      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      val proofs = fullBlock.aDProofs.get
      fullHistory.contains(header) shouldBe false
      fullHistory.contains(txs) shouldBe false
      fullHistory.contains(proofs) shouldBe false
      fullHistory.applicable(header) shouldBe true
      fullHistory.applicable(proofs) shouldBe false
      fullHistory.applicable(txs) shouldBe false

      fullHistory = fullHistory.append(header).get._1

      fullHistory.contains(header) shouldBe true
      fullHistory.contains(txs) shouldBe false
      fullHistory.contains(proofs) shouldBe false
      fullHistory.applicable(header) shouldBe false
      fullHistory.applicable(proofs) shouldBe true
      fullHistory.applicable(txs) shouldBe true
      fullHistory.bestHeader shouldBe header
      fullHistory.bestFullBlockOpt.get shouldBe startFullBlock
      fullHistory.openSurfaceIds().head shouldEqual startFullBlock.header.id

      fullHistory = fullHistory.append(txs).get._1

      fullHistory.contains(header) shouldBe true
      fullHistory.contains(txs) shouldBe true
      fullHistory.contains(proofs) shouldBe false
      fullHistory.applicable(header) shouldBe false
      fullHistory.applicable(proofs) shouldBe true
      fullHistory.applicable(txs) shouldBe false
      fullHistory.bestHeader shouldBe header
      fullHistory.bestFullBlockOpt.get shouldBe startFullBlock

      fullHistory = fullHistory.append(proofs).get._1

      fullHistory.contains(header) shouldBe true
      fullHistory.contains(txs) shouldBe true
      fullHistory.contains(proofs) shouldBe true
      fullHistory.applicable(header) shouldBe false
      fullHistory.applicable(proofs) shouldBe false
      fullHistory.applicable(txs) shouldBe false
      fullHistory.bestHeader shouldBe header
      fullHistory.bestFullBlockOpt.get shouldBe fullBlock
      fullHistory.openSurfaceIds().head shouldEqual fullBlock.header.id
    }
  }

  property("compare()") {
    //TODO what if headers1 > headers2 but fullchain 1 < fullchain2?
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

  private def generateHistory(verify: Boolean, adState: Boolean, toKeep: Int): ErgoHistory = {
    val paramsHash = Base58.encode(Algos.hash(verify.toString + adState + toKeep))
    val fullHistorySettings: ErgoSettings = new ErgoSettings {
      override def settingsJSON: Map[String, Json] = Map()

      override val verifyTransactions: Boolean = verify
      override val ADState: Boolean = adState
      override val dataDir: String = s"/tmp/ergo/test-history-$paramsHash"
      override val blocksToKeep: Int = toKeep
    }
    new File(fullHistorySettings.dataDir).mkdirs()
    ErgoHistory.readOrGenerate(fullHistorySettings)
  }

  private def historyTest(histories: Seq[ErgoHistory])(fn: ErgoHistory => Unit): Unit = histories.foreach(fn)


}
