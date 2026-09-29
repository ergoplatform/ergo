package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.validation.{ParentHeaderNotFoundError, RecoverableModifierError}
import org.scalatest.OptionValues
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, bytesToId}

import scala.annotation.tailrec

class ErgoModifiersCacheSpec extends ErgoCorePropertyTest with OptionValues {
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._
  private def genKey(i: Int): ModifierId = bytesToId(Blake2b256(s"$i"))

  private def genCachePair(i: Int): (ModifierId, Header) = {
    val header = defaultHeaderGen.sample.value
    val k = genKey(i)
    k -> header
  }

  property("cache size is within limits") {
    val limit = 3
    val modifiersCache = new ErgoModifiersCache(limit)

    modifiersCache.maxSize shouldBe limit

    (1 to limit).foreach { i =>
      val (k, h) = genCachePair(i)
      modifiersCache.put(k, h)
    }

    modifiersCache.size shouldBe limit

    val above = genCachePair(limit + 1)

    modifiersCache.put(above._1, above._2)
    modifiersCache.size shouldBe (limit + 1)

    modifiersCache.cleanOverfull()

    modifiersCache.size shouldBe limit

    modifiersCache.remove(genKey(1)).isEmpty shouldBe true
  }

  property("cache is proposing a reasonable candidate to enhance history") {
    val limit = 25
    val modifiersCache = new ErgoModifiersCache(limit)

    val history0 = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

    val chain = genChain(5, history0)

    chain.foreach { fb =>
      modifiersCache.put(fb.header.id, fb.header)
      modifiersCache.put(fb.header.transactionsId, fb.blockTransactions)
      modifiersCache.put(fb.header.ADProofsId, fb.adProofs.value)
    }

    //The history is empty - we can apply only a header at height == 0 at this moment.
    //Out of 15 elements in the cache, the cache should propose a proper candidate
    val c1 = modifiersCache.popCandidate(history0).value
    c1.isInstanceOf[Header] shouldBe true
    val h1 = c1.asInstanceOf[Header]
    h1.height shouldBe GenesisHeight

    val history1 = history0.append(c1).get._1

    //We have only header of height == 0 in the history, so cache should return whether a header of height == 1
    //or a non-header part of the full block at height == 0
    val c2 = modifiersCache.popCandidate(history1).value
    val properCandidate = c2 match {
      case h: Header => h.height == 1
      case bt: BlockTransactions => bt.id == h1.transactionsId
      case ap: ADProofs => ap.id == h1.ADProofsId
    }
    properCandidate shouldBe true
  }

  property("cache is proposing proper candidate during forking") {
    val limit = 25
    val modifiersCache = new ErgoModifiersCache(limit)

    var history = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

    val chain = genChain(1, history)

    chain.foreach{fb => history = applyBlock(history, fb)}

    val chain1 = genChain(5, history).tail

    val chain2 = genChain(10, history).tail

    chain1.foreach(fb => history = applyBlock(history, fb))

    chain2.foreach(fb => history = history.append(fb.header).get._1)

    history.bestFullBlockOpt.value shouldBe chain1.last
    history.bestHeaderOpt.value shouldBe chain2.last.header

    chain2.flatMap(_.blockSections).foreach(s => modifiersCache.put(s.id, s))

    @tailrec
    def applyLoop(): Unit = {
      modifiersCache.popCandidate(history) match {
        case Some(mod) =>
          history.append(mod)
          applyLoop()
        case None =>
          modifiersCache.size shouldBe 0
          history.bestFullBlockOpt.value shouldBe chain2.last
      }
    }
    applyLoop()
  }

  property("stuck-header recovery rotates across independent missing parents") {
    val modifiersCache = new ErgoModifiersCache(25)
    val history = generateHistory(
      verifyTransactions = true,
      StateType.Utxo,
      PoPoWBootstrap = false,
      BlocksToKeep
    )

    val firstBranch = genChain(2, history)
    val secondBranch = (1 to 10).iterator
      .map(_ => genChain(2, history))
      .find(_.head.header.id != firstBranch.head.header.id)
      .value
    val orphanHeaders = Seq(firstBranch.last.header, secondBranch.last.header)

    orphanHeaders.foreach(header => modifiersCache.put(header.id, header))

    val expectedParents = orphanHeaders.map(_.parentId).toSet
    val samePage = modifiersCache.findMissingParentIds(history, orphanHeaders, limit = orphanHeaders.size)
    samePage.toSet shouldBe expectedParents

    val firstReported = modifiersCache.findMissingParentIds(history, Seq.empty, limit = 1).head
    val secondReported = modifiersCache.findMissingParentIds(history, Seq.empty, limit = 1).head

    firstReported should not be secondReported
    Set(firstReported, secondReported) shouldBe expectedParents
  }

  property("a newly discovered missing parent is not starved by a previously reported parent") {
    val modifiersCache = new ErgoModifiersCache(25)
    val history = generateHistory(
      verifyTransactions = true,
      StateType.Utxo,
      PoPoWBootstrap = false,
      BlocksToKeep
    )
    val firstBranch = genChain(2, history)
    val secondBranch = (1 to 10).iterator
      .map(_ => genChain(2, history))
      .find(_.head.header.id != firstBranch.head.header.id)
      .value
    val firstOrphan = firstBranch.last.header
    val secondOrphan = secondBranch.last.header

    modifiersCache.put(firstOrphan.id, firstOrphan)
    modifiersCache.findMissingParentIds(history, Seq(firstOrphan), limit = 1) shouldBe
      Seq(firstOrphan.parentId)

    modifiersCache.put(secondOrphan.id, secondOrphan)
    modifiersCache.findMissingParentIds(history, Seq(secondOrphan), limit = 1) shouldBe
      Seq(secondOrphan.parentId)
  }

  property("stuck-header recovery reports only the external frontier of a cached chain") {
    val modifiersCache = new ErgoModifiersCache(25)
    val history = generateHistory(
      verifyTransactions = true,
      StateType.Utxo,
      PoPoWBootstrap = false,
      BlocksToKeep
    )
    val chain = genChain(3, history)
    val frontier = chain(1).header
    val descendant = chain(2).header

    Seq(frontier, descendant).foreach(header => modifiersCache.put(header.id, header))

    val reported = modifiersCache.findMissingParentIds(history, Seq(frontier, descendant), limit = 25)
    reported shouldBe Seq(chain.head.header.id)
  }

  property("stuck-header recovery reports one representative per missing parent") {
    val modifiersCache = new ErgoModifiersCache(25)
    val history = generateHistory(
      verifyTransactions = true,
      StateType.Utxo,
      PoPoWBootstrap = false,
      BlocksToKeep
    )
    val parent = genChain(1, history).head.header
    val firstChild = nextHeader(
      Some(parent),
      history.difficultyCalculator,
      tsOpt = Some(parent.timestamp + 1),
      useRealTs = false
    )
    val secondChild = nextHeader(
      Some(parent),
      history.difficultyCalculator,
      tsOpt = Some(parent.timestamp + 2),
      useRealTs = false
    )

    Seq(firstChild, secondChild).foreach(header => modifiersCache.put(header.id, header))

    val reported = modifiersCache.findMissingParentIds(history, Seq(firstChild, secondChild), limit = 25)
    reported shouldBe Seq(parent.id)
  }

  property("stuck-header recovery excludes non-parent recoverable errors") {
    val modifiersCache = new ErgoModifiersCache(25)
    val emptyHistory = generateHistory(
      verifyTransactions = true,
      StateType.Utxo,
      PoPoWBootstrap = false,
      BlocksToKeep
    )
    val parent = genChain(1, emptyHistory).head.header
    val history = emptyHistory.append(parent).get._1
    val futureHeader = nextHeader(
      Some(parent),
      history.difficultyCalculator,
      tsOpt = Some(System.currentTimeMillis() + 60L * 60L * 1000L),
      useRealTs = true
    )
    val failure = history.applicableTry(futureHeader).failed.toOption.value

    failure.isInstanceOf[RecoverableModifierError] shouldBe true
    failure.isInstanceOf[ParentHeaderNotFoundError] shouldBe false
    modifiersCache.put(futureHeader.id, futureHeader)

    modifiersCache.findMissingParentIds(history, Seq(futureHeader), limit = 25) shouldBe empty
  }

  property("missing-parent recovery suppresses cached parents and restores evicted frontiers") {
    val modifiersCache = new ErgoModifiersCache(25)
    val history = generateHistory(
      verifyTransactions = true,
      StateType.Utxo,
      PoPoWBootstrap = false,
      BlocksToKeep
    )
    val chain = genChain(2, history)
    val parent = chain.head.header
    val child = chain.last.header

    modifiersCache.put(child.id, child)
    modifiersCache.findMissingParentIds(history, Seq(child), limit = 25) shouldBe Seq(parent.id)

    modifiersCache.put(parent.id, parent)
    modifiersCache.findMissingParentIds(history, Seq.empty, limit = 25) shouldBe empty

    modifiersCache.remove(parent.id).value shouldBe parent
    modifiersCache.findMissingParentIds(history, Seq.empty, limit = 25) shouldBe Seq(parent.id)

    modifiersCache.remove(child.id).value shouldBe child
    modifiersCache.findMissingParentIds(history, Seq.empty, limit = 25) shouldBe empty
  }

  property("missing-parent recovery excludes headers removed by overfull cleanup") {
    val modifiersCache = new ErgoModifiersCache(1)
    val history = generateHistory(
      verifyTransactions = true,
      StateType.Utxo,
      PoPoWBootstrap = false,
      BlocksToKeep
    )
    val firstBranch = genChain(2, history)
    val secondBranch = (1 to 10).iterator
      .map(_ => genChain(2, history))
      .find(_.head.header.id != firstBranch.head.header.id)
      .value
    val firstOrphan = firstBranch.last.header
    val secondOrphan = secondBranch.last.header

    modifiersCache.put(firstOrphan.id, firstOrphan)
    modifiersCache.findMissingParentIds(history, Seq(firstOrphan), limit = 1) shouldBe Seq(firstOrphan.parentId)
    modifiersCache.put(secondOrphan.id, secondOrphan)

    modifiersCache.cleanOverfull() should contain only firstOrphan
    modifiersCache.findMissingParentIds(history, Seq(firstOrphan, secondOrphan), limit = 25) shouldBe
      Seq(secondOrphan.parentId)
  }

}
