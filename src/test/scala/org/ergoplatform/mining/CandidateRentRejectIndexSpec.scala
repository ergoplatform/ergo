package org.ergoplatform.mining

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.ByteBuffer

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexer, IndexedErgoBox, NumericBoxIndex, StorageRentBox}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoStateContext, UtxoState, VotingData}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigmastate.helpers.TestingHelpers._

import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

/** A rejected batch claim must not erase eligibility of independent unspent boxes. */
class CandidateRentRejectIndexSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}

  property("prioritized owner spend does not drop other boxes in a rejected rent claim") {
    val height = 3 * Constants.StoragePeriod
    val spentByOwner = testBox(10000000000L, Constants.TrueTree,
      height - Constants.StoragePeriod, Seq.empty, Map.empty)
    val stillUnspent = testBox(11000000000L, Constants.TrueTree,
      height - Constants.StoragePeriod, Seq.empty, Map.empty)
    val freshBox = testBox(12000000000L, Constants.TrueTree,
      height - 1, Seq.empty, Map.empty)
    val fixtureSettings = baseSettings.copy(
      directory = createTempDir.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = true))

    val baseState = UtxoState.fromBoxHolder(BoxHolder(Seq(spentByOwner, stillUnspent, freshBox)), None,
      createTempDir, fixtureSettings, parameters)
    val block = invalidErgoFullBlockGen.sample.get
    val parentHeader = block.header.copy(height = height - 1)
    val parentBlock = block.copy(parentHeader)
    val agedContext = new ErgoStateContext(Seq(parentHeader), None,
      genesisStateDigest, parameters, validationSettingsNoIl, VotingData.empty)(fixtureSettings.chainSettings)
    val state = new UtxoState(baseState.persistentProver, baseState.version,
      baseState.store, fixtureSettings) {
      override def stateContext: ErgoStateContext = agedContext
      override def emissionBoxOpt: Option[ErgoBox] = None
    }

    val storage = HistoryStorage(fixtureSettings)
    Seq(spentByOwner, stillUnspent).zipWithIndex.foreach { case (box, index) =>
      val indexed = new IndexedErgoBox(box.creationHeight, None, None, None, box, index.toLong)
      storage.insertExtra(Array.empty[(Array[Byte], Array[Byte])],
        Array[ExtraIndex](indexed, NumericBoxIndex(index.toLong, indexed.id), StorageRentBox(indexed)))
    }

    storage.insertExtra(Array(ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(height - 1).array),
      Array.empty[ExtraIndex])

    val history = Proxy.newProxyInstance(
      classOf[ErgoHistoryReader].getClassLoader,
      Array[Class[_]](classOf[ErgoHistoryReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "bestFullBlockOpt" => Some(parentBlock)
            case "fullBlockHeight" => Int.box(parentHeader.height)
            case "historyStorage" => storage
            case "typedExtraIndexById" => storage.getExtraIndex(args(0).asInstanceOf[ModifierId])
            case "storageRentBoxesAtOrBefore" =>
              storage.storageRentBoxesAtOrBefore(args(0).asInstanceOf[Int], args(1).asInstanceOf[Int])
            case "removeStorageRentBoxes" =>
              storage.removeStorageRentBoxes(args(0).asInstanceOf[Seq[ModifierId]])
              ().asInstanceOf[AnyRef]
            case "typedModifierById" => None
            case "requiredDifficultyAfter" => parentHeader.requiredDifficulty
            case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
          }
      }
    ).asInstanceOf[ErgoHistoryReader]

    try {
      val threshold = height - Constants.StoragePeriod
      val before = history.storageRentBoxesAtOrBefore(threshold, StorageRentClaimBuilder.MaxClaims)
      before.map(_.globalIndex).toSet shouldBe Set(0L, 1L)
      val batch = StorageRentClaimBuilder.buildClaim(Seq(spentByOwner, stillUnspent),
        height, parameters, defaultMinerPk, None, Set.empty[ModifierId]).get
      batch.inputs.map(in => bytesToId(in.boxId)).toSet shouldBe
        Set(bytesToId(spentByOwner.id), bytesToId(stillUnspent.id))

      val ownerSpend = ErgoTransaction(
        IndexedSeq(Input(spentByOwner.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(spentByOwner.value, Constants.TrueTree, height)))
      val result = CandidateGenerator.generateCandidate(history, state,
        ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(ownerSpend), None, fixtureSettings)
      result should not be None
      result.get.isSuccess shouldBe true
      val (candidate, rejected) = result.get.get
      candidate.candidateBlock.transactions.map(_.id) should contain(ownerSpend.id)
      candidate.candidateBlock.transactions.map(_.id) should not contain batch.id
      rejected.ids should contain(batch.id)

      // A was spent only in this candidate; B was never spent at all. It must remain
      // available for the next candidate even when the combined claim was rejected.
      state.boxById(ADKey @@ idToBytes(bytesToId(stillUnspent.id))) shouldBe Some(stillUnspent)
      history.storageRentBoxesAtOrBefore(threshold, StorageRentClaimBuilder.MaxClaims)
        .map(_.globalIndex).toSet shouldBe Set(0L, 1L)

      // Neither candidate has been applied. With the conflict gone, the ordinary next
      // generation must rediscover and include the claim without repairing the index.
      // An independent young-box spend satisfies the existing nonempty-candidate gate.
      val independentSpend = ErgoTransaction(
        IndexedSeq(Input(freshBox.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(freshBox.value, Constants.TrueTree, height)))
      val nextResult = CandidateGenerator.generateCandidate(history, state,
        ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(independentSpend), None, fixtureSettings)
      val (nextCandidate, nextRejected) = nextResult.get.get
      nextCandidate.candidateBlock.transactions.map(_.id) should contain(batch.id)
      nextRejected.ids should not contain batch.id
      state.boxById(spentByOwner.id) shouldBe Some(spentByOwner)
      state.boxById(stillUnspent.id) shouldBe Some(stillUnspent)
      history.storageRentBoxesAtOrBefore(threshold, StorageRentClaimBuilder.MaxClaims)
        .map(_.globalIndex).toSet shouldBe Set(0L, 1L)
    } finally {
      storage.close()
      state.closeStorage()
    }
  }
}
