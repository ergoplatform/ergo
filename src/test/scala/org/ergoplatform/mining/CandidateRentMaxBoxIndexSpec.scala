package org.ergoplatform.mining

import java.lang.reflect.{InvocationHandler, Method, Proxy}

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexer, IndexedErgoBox, NumericBoxIndex, StorageRentBox}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoStateContext, UtxoState, VotingData}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.data.Digest32Coll
import sigmastate.helpers.TestingHelpers._

import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

class CandidateRentMaxBoxIndexSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}

  property("a narrower whitelist can later claim an oversized skipped box") {
    val height = 3 * Constants.StoragePeriod
    val value = 10000000L
    val dummyTxId = bytesToId(Array.fill(32)(0.toByte))
    val witness = (for {
      tokenCount <- 90 to 130
      firstAmount <- Seq(1L, 128L, 16384L, 268435456L)
    } yield {
      val tokens = (1 to tokenCount).map { i =>
        val amount = if (i == 1) firstAmount else 1L
        (Digest32Coll @@ Colls.fromArray(Array.fill(32)(i.toByte))) -> amount
      }
      val box = testBox(value, Constants.TrueTree,
        height - Constants.StoragePeriod, tokens, Map.empty)
      val proceeds = new ErgoBoxCandidate(value,
        sigma.ast.ErgoTree.fromSigmaBoolean(defaultMinerPk), height,
        box.additionalTokens, Map.empty)
      (box, proceeds)
    }).find { case (box, proceeds) =>
      val fee = parameters.storageFeeFactor * box.bytes.length
      box.bytes.length <= ErgoBox.MaxBoxSize &&
        box.value > parameters.minValuePerByte.toLong * box.bytes.length &&
        fee > 0 && box.value <= fee &&
        proceeds.toBox(dummyTxId, 0).bytes.length > ErgoBox.MaxBoxSize
    }.getOrElse(fail("bounded token-count search found no near-limit input"))
    val box = witness._1
    val unrelated = testBox(10000000000L, Constants.TrueTree, height,
      Seq.empty, Map.empty)
    val unrelatedSpend = ErgoTransaction(
      IndexedSeq(Input(unrelated.id, emptyProverResult)),
      IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(unrelated.value, Constants.TrueTree, height)))
    val dir = createTempDir
    val fixtureSettings = baseSettings.copy(
      directory = dir.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(
        extraIndex = true,
        storageRentCollection = true,
        storageRentTokenWhitelist = box.tokens.keys.toSeq.map(_.toString)))
    val baseState = UtxoState.fromBoxHolder(BoxHolder(Seq(box, unrelated)), None,
      dir, fixtureSettings, parameters)
    val block = invalidErgoFullBlockGen.sample.get
    val parentHeader = block.header.copy(height = height - 1)
    val parentBlock = block.copy(parentHeader)
    val agedContext = new ErgoStateContext(Seq(parentHeader), None,
      genesisStateDigest, parameters, validationSettingsNoIl,
      VotingData.empty)(fixtureSettings.chainSettings)
    val state = new UtxoState(baseState.persistentProver, baseState.version,
      baseState.store, fixtureSettings) {
      override def stateContext: ErgoStateContext = agedContext
      override def emissionBoxOpt: Option[ErgoBox] = None
    }
    val storage = HistoryStorage(fixtureSettings)
    val indexed = new IndexedErgoBox(box.creationHeight, None, None, None, box, 0L)
    storage.insertExtra(Array(
      ExtraIndexer.IndexedHeightKey ->
        java.nio.ByteBuffer.allocate(4).putInt(parentHeader.height).array,
      ExtraIndexer.GlobalBoxIndexKey -> java.nio.ByteBuffer.allocate(8).putLong(1L).array
    ),
      Array[ExtraIndex](indexed, NumericBoxIndex(0L, indexed.id), StorageRentBox(indexed)))
    val history = historyProxy(storage, parentBlock)

    try {
      val cutoff = height - Constants.StoragePeriod
      def indexedRentBoxIds: Set[ModifierId] =
        history.storageRentBoxesAtOrBefore(cutoff, StorageRentClaimBuilder.MaxClaims, None)
          .flatMap(entry => NumericBoxIndex.getBoxByNumber(history, entry.globalIndex))
          .map(_.id).toSet
      indexedRentBoxIds should contain(bytesToId(box.id))
      val result = CandidateGenerator.generateCandidate(history, state,
        ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(unrelatedSpend),
        None, fixtureSettings)
      result should not be None
      withClue(s"first candidate result=$result: ") { result.get.isSuccess shouldBe true }
      result.get.get._1.candidateBlock.transactions.map(_.id) should contain(unrelatedSpend.id)
      result.get.get._1.candidateBlock.transactions.flatMap(_.inputs)
        .exists(_.boxId.sameElements(box.id)) shouldBe false
      // The first candidate cannot include the claim, but it has not spent the box.
      state.boxById(box.id).isDefined shouldBe true
      indexedRentBoxIds should contain(bytesToId(box.id))

      // With fewer protected tokens, the same UTXO now has a valid burn claim.
      val narrowSettings = fixtureSettings.copy(nodeSettings =
        fixtureSettings.nodeSettings.copy(storageRentTokenWhitelist = Seq.empty))
      val directClaim = StorageRentClaimBuilder.buildClaim(Iterator(box), height,
        parameters, defaultMinerPk, None, Set.empty)
      directClaim.isDefined shouldBe true

      // Candidate generation must rediscover that unspent box after the policy change.
      val retry = CandidateGenerator.generateCandidate(history, state,
        ErgoMemPool.empty(narrowSettings), defaultMinerPk, Seq(unrelatedSpend),
        None, narrowSettings)
      retry should not be None
      retry.get.isSuccess shouldBe true
      val retryClaimed = retry.get.get._1.candidateBlock.transactions.flatMap(_.inputs)
        .exists(_.boxId.sameElements(box.id))
      indexedRentBoxIds should contain(bytesToId(box.id))
      retryClaimed shouldBe true
    } finally {
      storage.close()
      state.closeStorage()
    }
  }

  private def historyProxy(storage: HistoryStorage,
                           parentBlock: ErgoFullBlock): ErgoHistoryReader =
    Proxy.newProxyInstance(
      classOf[ErgoHistoryReader].getClassLoader,
      Array[Class[_]](classOf[ErgoHistoryReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "historyStorage" => storage
            case "fullBlockHeight" => java.lang.Integer.valueOf(parentBlock.header.height)
            case "bestFullBlockOpt" => Some(parentBlock)
            case "isInSelectedFullChain" =>
              java.lang.Boolean.valueOf(args(0) == parentBlock.id)
            case "storageRentScanPage" =>
              storage.storageRentScanPage(args(0).asInstanceOf[Int],
                args(1).asInstanceOf[Long], args(2).asInstanceOf[Option[Array[Byte]]],
                args(3).asInstanceOf[Int])
            case "storageRentBoxesAtOrBefore" =>
              storage.storageRentBoxesAtOrBefore(args(0).asInstanceOf[Int],
                args(1).asInstanceOf[Int], args(2).asInstanceOf[Option[StorageRentBox]])
            case "removeStorageRentBoxes" =>
              storage.removeStorageRentBoxes(args(0).asInstanceOf[Seq[ModifierId]])
              ().asInstanceOf[AnyRef]
            case "typedExtraIndexById" =>
              storage.getExtraIndex(args(0).asInstanceOf[ModifierId])
            case "typedModifierById" => None
            case "requiredDifficultyAfter" => parentBlock.header.requiredDifficulty
            case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
          }
      }
    ).asInstanceOf[ErgoHistoryReader]
}
