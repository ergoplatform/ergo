package org.ergoplatform.nodeView.history.modifierprocessors

import com.google.common.io.Files.createTempDir
import org.ergoplatform.ErgoBox
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.nodeView.state.{BoxHolder, StateType, UtxoState}
import org.ergoplatform.settings.Algos
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.hash.Digest32
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.TrivialProp.TrueProp

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}

/**
  * The node view holder applies input blocks to the history while the synchronizer reads the same history from its
  * own thread (e.g. `getInputBlock` on `NewBestInputBlock`). An input block already stored must stay visible to a
  * reader while other input blocks are being applied.
  */
class InputBlockProcessorConcurrencySpecification extends ErgoCorePropertyTest {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val box = new ErgoBox(
    value = 1000000000L,
    ergoTree = ErgoTree.fromProposition(TrueProp),
    creationHeight = 0,
    additionalTokens = Colls.emptyColl,
    additionalRegisters = Map.empty,
    transactionId = bytesToId(Algos.hash("concurrencyTx")),
    index = 0
  )

  // a previous input block this history does not know: the announcement goes to the disconnected waitlist
  private def unknownPrev(i: Int): InputBlockFields = {
    new InputBlockFields(
      Some(Algos.hash(s"unknown-prev-$i")),
      Digest32 @@ Array.fill(32)(0.toByte),
      Digest32 @@ Array.fill(32)(0.toByte),
      BatchMerkleProof(Seq.empty, Seq.empty)(Algos.hash))
  }

  property("a stored input block stays visible to a reader while other input blocks are applied") {
    val us = UtxoState.fromBoxHolder(BoxHolder(Seq(box)), None, createTempDir, settings, parameters)
    val h = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1,
      epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, None)
    applyChain(h, genChain(2, h, stateOpt = Some(us)))
    val template = genChain(2, h, stateOpt = Some(us)).tail.head.header

    val target = InputBlockAnnouncement(1, template, InputBlockFields.empty, None)
    h.applyInputBlock(target)
    h.getInputBlock(target.id) shouldBe Some(target)

    val others = (1 to 20000).map { i =>
      InputBlockAnnouncement(1, template.copy(timestamp = template.timestamp + i), unknownPrev(i), None)
    }

    val writing = new AtomicBoolean(true)
    val misses = new AtomicLong(0)
    val reads = new AtomicLong(0)
    val readerError = new AtomicReference[Throwable](null)
    val reader = new Thread(() => {
      try {
        while (writing.get()) {
          reads.incrementAndGet()
          if (h.getInputBlock(target.id).isEmpty) misses.incrementAndGet()
        }
      } catch {
        case t: Throwable => readerError.set(t)
      }
    })
    reader.start()
    try {
      others.foreach(h.applyInputBlock)
    } finally {
      writing.set(false)
      reader.join()
    }

    Option(readerError.get()) shouldBe None
    h.getInputBlock(target.id) shouldBe Some(target)
    reads.get() should be > 0L
    withClue(s"${misses.get()} of ${reads.get()} concurrent reads missed the stored input block: ") {
      misses.get() shouldBe 0L
    }
  }
}
