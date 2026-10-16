package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, Props}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.IndexedContractTemplateSerializer.hashTreeTemplate
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.state.{StateType, UtxoStateReader}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, CheckpointSettings, Constants, ErgoSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock, validTransactionsFromBoxHolder}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, RandomWrapper, untilTimeout}
import scorex.util.{ModifierId, bytesToId}
import scorex.util.encode.Base16
import sigma.ast.ErgoTree
import sigma.interpreter.ProverResult
import sigma.serialization.ErgoTreeSerializer

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

/**
  * When an [[sigma.ast.ErgoTree]] carries the size flag but its constant section can not be read,
  * the deserializer keeps it as unparsed bytes and `tree.template` throws. The fallback template
  * key in [[IndexedContractTemplateSerializer.hashTreeTemplate]] must then differ from the address
  * key of the same tree ([[IndexedErgoAddressSerializer.hashErgoTree]]): they share one key space
  * (the record type is in the value, not the key) and are written in one batch, so an equal key
  * loses the address record to the template record. These tests pin the key derivation and run the
  * production [[ExtraIndexer]] against a real node view holder (history and UTXO state) to check
  * that both records exist, each under its own key, after payments, a spend and chain switches.
  */
class ExtraIndexerTemplateKeySpec extends ErgoCorePropertyTest with NodeViewTestOps {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private val settleTimeout = 15.seconds

  private implicit val segmentThreshold: Int = 512 // as the production indexer

  private val indexedSettings: ErgoSettings = {
    val base = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    base.copy(nodeSettings = base.nodeSettings.copy(extraIndex = true))
  }

  /**
    * The same node with a checkpoint above the test chain. Below the checkpoint height the state
    * applies blocks without checking their transactions (`ErgoState.execTransactions`), as on
    * mainnet below its checkpoint. The test needs this to put a spend of a box with an unparsed
    * tree into a block; the indexer does not read this setting.
    */
  private val checkpointedSettings: ErgoSettings = indexedSettings.copy(nodeSettings =
    indexedSettings.nodeSettings.copy(checkpoint = Some(CheckpointSettings(1000, bytesToId(Array.fill(32)(0: Byte))))))

  private def treeFromBytes(hex: String): ErgoTree =
    ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(hex).get)

  /** Size flag and constant segregation, one constant whose type code no script version knows. */
  private val unparsedTree: ErgoTree = treeFromBytes("180401ff0000")
  private val addressKey = hashErgoTree(unparsedTree)
  private val templateKey = hashTreeTemplate(unparsedTree)

  property("hashTreeTemplate: templates of parseable trees keep the base keys") {
    // normal path hash(tree.template); these are computed on the base and must not move
    val burn = treeFromBytes("10010100d17300") // mainnet burn tree 4MQyMKvMbnCJG3aJ, parses
    hashTreeTemplate(burn).toString shouldBe "f2f223f095d72c2431b7071f15f1880dcd4bbba7791b763338b5a15b517bf174"
    hashErgoTree(burn).toString shouldBe "a3b99d2d8c80b28f6804769775e24d18f98cb8835500fff12b6f8b59c6469eb8"
    // a different tree with the same template keeps the same template key and a different address key
    val trueTree = Constants.TrueTree
    hashTreeTemplate(trueTree).toString shouldBe "f2f223f095d72c2431b7071f15f1880dcd4bbba7791b763338b5a15b517bf174"
    hashErgoTree(trueTree) should not be hashErgoTree(burn)
  }

  property("hashTreeTemplate: the fallback key of an unreadable tree is deterministic and never its address key") {
    Seq("180401ff0000", "1804016f0000", "180401700000").foreach { hex =>
      val tree = treeFromBytes(hex)
      tree.root.isRight shouldBe false // kept as unparsed bytes
      val templateKey = hashTreeTemplate(tree)
      templateKey shouldBe hashTreeTemplate(tree) // deterministic
      templateKey should not be hashErgoTree(tree) // the defect: these were equal on the base
      // the derivation: the address key hashed again with a suffix, as tokens separate their key
      templateKey shouldBe bytesToId(Algos.hash(hashErgoTree(tree) + "template"))
    }
    templateKey.toString shouldBe "b26e420014c341c78e4f3441df9cf5a18311a31d996b7aa8017336c7330cbcca"
    hashTreeTemplate(treeFromBytes("1804016f0000")).toString shouldBe "843ab2d9a89c2b37857f0ecb7d0be635c0722815c1dd08fbf5ce94a6fa580c07"
    hashTreeTemplate(treeFromBytes("180401700000")).toString shouldBe "f19853ac3cdad1df6c01b2c49fa30db01fd33b21941e3045ee55b9e78f776b8b"
  }

  /** A real node view holder and the production indexer, as in `ExtraIndexerAppliedBlocksSpec`. */
  private class Node(fixture: NodeViewFixture) {

    import fixture._

    /** Blocks the state has applied. */
    val chain: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty[ErgoFullBlock]

    /** Copy of the state after the last block of `chain`, to build further blocks with. */
    var tipState: WrappedUtxoState = {
      val (genesisState, boxHolder) = createUtxoState(settings)
      val genesis = validFullBlock(None, genesisState, boxHolder)
      val afterGenesis = WrappedUtxoState(genesisState, boxHolder, settings).applyModifier(genesis)(_ => ()).get
      applyBlock(genesis) shouldBe 'success
      chain += genesis
      afterGenesis
    }
    (1 to 3).foreach(_ => extendWith(validTransactionsFromBoxHolder(tipState.versionedBoxHolder, new RandomWrapper)._1))

    private val indexer: ActorRef = actorSystem.actorOf(Props(
      new ExtraIndexer(settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), true)))
    awaitIndexed(chain.last)

    def history: ErgoHistory = getHistory

    def state: UtxoStateReader = getCurrentState.asInstanceOf[UtxoStateReader]

    def stopIndexer(): Unit = actorSystem.stop(indexer)

    def deliver(block: ErgoFullBlock): Unit = applyBlock(block) shouldBe 'success

    def awaitApplied(block: ErgoFullBlock): Unit = untilTimeout(settleTimeout, 20.millis) {
      getHistory.bestFullBlockIdOpt shouldBe Some(block.id)
      getCurrentState.version shouldBe idToVersion(block.id)
    }

    def awaitIndexed(block: ErgoFullBlock): Unit = untilTimeout(settleTimeout, 20.millis) {
      val checkpoint = IndexerState.fromHistory(getHistory)
      checkpoint.indexedHeaderId shouldBe Some(block.id)
      checkpoint.indexedHeight shouldBe block.header.height
      checkpoint.rollbackTo shouldBe 0
    }

    /** One more block with the given transactions, handed over the usual way, applied and indexed. */
    def extendWith(txs: Seq[ErgoTransaction]): ErgoFullBlock = {
      val block = validFullBlock(Some(chain.last), tipState, txs)
      deliver(block)
      awaitApplied(block)
      tipState = tipState.applyModifier(block)(_ => ()).get
      chain += block
      block
    }

    /**
      * A block of generated transactions and a last one which pays one box to the unparsed tree.
      * After it the state holds a box whose proposition can not be read, which the transaction
      * generators do not survive: later blocks are built by hand from `spare`.
      *
      * @return the box of the tree and an unspent box anyone can spend
      */
    def firstPayment(): (ErgoBox, ErgoBox) = {
      val txs = validTransactionsFromBoxHolder(tipState.versionedBoxHolder, new RandomWrapper)._1
      val spent = txs.last.outputs.head
      val inputIds = txs.flatMap(_.inputs).map(input => bytesToId(input.boxId)).toSet + bytesToId(spent.id)
      val spare = txs.flatMap(_.outputs).find(out => out.ergoTree == Constants.TrueTree && !inputIds(bytesToId(out.id))).get
      val pay = move(spent, unparsedTree)
      awaitIndexed(extendWith(txs :+ pay))
      pay.outputs.head -> spare
    }

    /**
      * Hands the node `abandoned`, a child of the tip, waits until it is indexed, runs `check`, and
      * then hands it a better branch of two blocks from the same parent: the first moves `spare`,
      * the second moves it again.
      */
    def switchAwayFrom(abandoned: ErgoFullBlock, spare: ErgoBox)(check: => Unit): Unit = {
      val first = move(spare, Constants.TrueTree)
      val b1 = validFullBlock(Some(chain.last), tipState, Seq(first), Some(abandoned.header.timestamp + 1))
      val afterB1 = tipState.applyModifier(b1)(_ => ()).get
      val b2 = validFullBlock(Some(b1), afterB1, Seq(move(first.outputs.head, Constants.TrueTree)))
      val afterB2 = afterB1.applyModifier(b2)(_ => ()).get
      deliver(abandoned)
      awaitApplied(abandoned)
      awaitIndexed(abandoned)
      check
      deliver(b1)
      deliver(b2)
      awaitApplied(b2)
      chain ++= Seq(b1, b2)
      tipState = afterB2
      awaitIndexed(b2)
    }

    def boxRecord(box: ErgoBox): Option[IndexedErgoBox] = getHistory.typedExtraIndexById[IndexedErgoBox](bytesToId(box.id))

    def addressRecord: Option[IndexedErgoAddress] = getHistory.typedExtraIndexById[IndexedErgoAddress](addressKey)

    def templateRecord: Option[IndexedContractTemplate] = getHistory.typedExtraIndexById[IndexedContractTemplate](templateKey)

    /** Ids of the transactions the address record lists. */
    def addressTxs: Seq[ModifierId] = addressRecord.toSeq.flatMap(_.txs).flatMap(NumericTxIndex.getTxByNumber(getHistory, _)).map(_.id)
  }

  /** Spends `box` with an empty proof and pays its whole value to `tree`. */
  private def move(box: ErgoBox, tree: ErgoTree): ErgoTransaction =
    ErgoTransaction(IndexedSeq(Input(box.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(box.value, tree, box.creationHeight, box.additionalTokens)))

  private def withNode(settings: ErgoSettings = indexedSettings)(test: Node => Unit): Unit =
    new NodeViewFixture(settings, parameters).apply { fixture =>
      val node = new Node(fixture)
      try test(node) finally {
        node.stopIndexer()
        node.tipState.closeStorage() // all copies of the state share one store
      }
    }

  property("a payment to a tree with an unreadable constant section is indexed by address and by template") {
    withNode() { node =>
      val (box, _) = node.firstPayment()
      val number = node.boxRecord(box).get.globalIndex

      node.state.boxById(box.id).isDefined shouldBe true
      node.addressRecord.map(_.boxes.toList) shouldBe Some(List(number))
      node.addressRecord.flatMap(_.balanceInfo).map(_.nanoErgs) shouldBe Some(box.value)
      node.addressTxs shouldBe Seq(box.transactionId)
      node.templateRecord.map(_.boxes.toList) shouldBe Some(List(number))
      // what the routes by address and by template hash serve
      node.addressRecord.get.retrieveBoxes(node.history, 0, 10).map(_.id).toList shouldBe List(bytesToId(box.id))
      node.templateRecord.get.retrieveBoxes(node.history, 0, 10).map(_.id).toList shouldBe List(bytesToId(box.id))
    }
  }

  property("a second payment to such a tree in a later block adds to its address record and its template record") {
    withNode() { node =>
      val (first, spare) = node.firstPayment()
      val pay = move(spare, unparsedTree)
      node.awaitIndexed(node.extendWith(Seq(pay)))
      val second = pay.outputs.head
      val numbers = List(first, second).map(box => node.boxRecord(box).get.globalIndex)

      List(first, second).foreach(box => node.state.boxById(box.id).isDefined shouldBe true)
      node.addressRecord.map(_.boxes.toList) shouldBe Some(numbers)
      node.addressRecord.flatMap(_.balanceInfo).map(_.nanoErgs) shouldBe Some(first.value + second.value)
      node.addressTxs shouldBe List(first, second).map(_.transactionId)
      node.templateRecord.map(_.boxes.toList) shouldBe Some(numbers)
    }
  }

  property("a chain switch which drops a payment to such a tree takes the box out of its address record and its template record") {
    withNode() { node =>
      val (first, spare) = node.firstPayment()
      val number = node.boxRecord(first).get.globalIndex
      val pay = move(spare, unparsedTree)
      val abandoned = validFullBlock(Some(node.chain.last), node.tipState, Seq(pay))
      node.switchAwayFrom(abandoned, spare) {
        node.addressRecord.map(_.boxes.size) shouldBe Some(2)
        node.templateRecord.map(_.boxes.size) shouldBe Some(2)
      }

      node.state.boxById(pay.outputs.head.id).isDefined shouldBe false
      node.boxRecord(pay.outputs.head) shouldBe None
      node.addressRecord.map(_.boxes.toList) shouldBe Some(List(number))
      node.addressRecord.flatMap(_.balanceInfo).map(_.nanoErgs) shouldBe Some(first.value)
      node.addressTxs shouldBe Seq(first.transactionId)
      node.templateRecord.map(_.boxes.toList) shouldBe Some(List(number))
    }
  }

  property("a chain switch which drops the only payment to such a tree removes its address record and its template record") {
    withNode() { node =>
      val txs = validTransactionsFromBoxHolder(node.tipState.versionedBoxHolder, new RandomWrapper)._1
      val pay = move(txs.last.outputs.head, unparsedTree)
      val abandoned = validFullBlock(Some(node.chain.last), node.tipState, txs :+ pay)
      val kept = validFullBlock(Some(node.chain.last), node.tipState, txs, Some(abandoned.header.timestamp + 1))
      node.deliver(abandoned)
      node.awaitApplied(abandoned)
      node.awaitIndexed(abandoned)
      node.addressRecord.map(_.boxes.size) shouldBe Some(1)
      node.templateRecord.map(_.boxes.size) shouldBe Some(1)

      val afterKept = node.tipState.applyModifier(kept)(_ => ()).get
      val tip = validFullBlock(Some(kept), afterKept)
      node.deliver(kept)
      node.deliver(tip)
      node.awaitApplied(tip)
      node.chain ++= Seq(kept, tip)
      node.tipState = afterKept.applyModifier(tip)(_ => ()).get
      node.awaitIndexed(tip)

      node.boxRecord(pay.outputs.head) shouldBe None
      node.addressRecord shouldBe None
      node.templateRecord shouldBe None
    }
  }

  property("a spend of a box of such a tree and the rollback of that spend update its address record and its template record") {
    withNode(checkpointedSettings) { node =>
      val (box, spare) = node.firstPayment()
      val number = node.boxRecord(box).get.globalIndex
      val spend = move(box, Constants.TrueTree)
      val abandoned = validFullBlock(Some(node.chain.last), node.tipState, Seq(spend))
      node.switchAwayFrom(abandoned, spare) {
        node.state.boxById(box.id).isDefined shouldBe false
        node.boxRecord(box).flatMap(_.spendingTxIdOpt) shouldBe Some(spend.id)
        node.addressRecord.map(_.boxes.toList) shouldBe Some(List(-number))
        node.addressRecord.flatMap(_.balanceInfo).map(_.nanoErgs) shouldBe Some(0L)
        node.addressTxs shouldBe Seq(box.transactionId, spend.id)
        node.templateRecord.map(_.boxes.toList) shouldBe Some(List(-number))
      }

      node.state.boxById(box.id).isDefined shouldBe true
      node.boxRecord(box).flatMap(_.spendingTxIdOpt) shouldBe None
      node.addressRecord.map(_.boxes.toList) shouldBe Some(List(number))
      node.addressRecord.flatMap(_.balanceInfo).map(_.nanoErgs) shouldBe Some(box.value)
      node.templateRecord.map(_.boxes.toList) shouldBe Some(List(number))
    }
  }

  property("an index written before the fix: the next payment to such a tree starts a new address record over the old template record") {
    withNode() { node =>
      val (first, spare) = node.firstPayment()
      val firstNumber = node.boxRecord(first).get.globalIndex
      // the records as the indexer wrote them before the fix: a template record under the address
      // key, which lists the box, no address record and nothing under the new template key
      val storage = node.history.historyStorage
      storage.removeExtra(Array(templateKey))
      storage.insertExtraTry(Array.empty, Array[ExtraIndex](IndexedContractTemplate(addressKey, ArrayBuffer(firstNumber)))).get
      node.addressRecord shouldBe None
      node.templateRecord shouldBe None
      // until the next payment the old record is still read under the old key
      node.history.typedExtraIndexById[IndexedContractTemplate](addressKey).map(_.boxes.toList) shouldBe Some(List(firstNumber))

      val pay = move(spare, unparsedTree)
      node.awaitIndexed(node.extendWith(Seq(pay)))
      val second = pay.outputs.head
      val secondNumber = node.boxRecord(second).get.globalIndex

      // both records now exist under their own keys and hold the new box only; the old template
      // record is overwritten, so the first box is in no list until the index is rebuilt
      node.addressRecord.map(_.boxes.toList) shouldBe Some(List(secondNumber))
      node.addressRecord.flatMap(_.balanceInfo).map(_.nanoErgs) shouldBe Some(second.value)
      node.addressTxs shouldBe Seq(second.transactionId)
      node.templateRecord.map(_.boxes.toList) shouldBe Some(List(secondNumber))
      node.history.typedExtraIndexById[IndexedContractTemplate](addressKey) shouldBe None
      node.boxRecord(first).map(_.globalIndex) shouldBe Some(firstNumber)
    }
  }

  property("an index written before the fix: until the next payment the old template record is not updated") {
    withNode(checkpointedSettings) { node =>
      val (first, spare) = node.firstPayment()
      val firstNumber = node.boxRecord(first).get.globalIndex
      val pay = move(spare, unparsedTree)
      val abandoned = validFullBlock(Some(node.chain.last), node.tipState, Seq(pay))
      var droppedNumber = 0L
      node.switchAwayFrom(abandoned, spare) {
        droppedNumber = node.boxRecord(pay.outputs.head).get.globalIndex
        // the records as the indexer wrote them before the fix: one template record under the address key
        node.history.historyStorage.insertExtraTry(Array.empty[(Array[Byte], Array[Byte])],
          Array[ExtraIndex](IndexedContractTemplate(addressKey, ArrayBuffer(firstNumber, droppedNumber))), Array(templateKey)).get
        node.addressRecord shouldBe None
        node.templateRecord shouldBe None
      }
      def oldRecord: Option[List[Long]] = node.history.typedExtraIndexById[IndexedContractTemplate](addressKey).map(_.boxes.toList)

      // the chain switch dropped the second box; its number stays in the old record
      node.boxRecord(pay.outputs.head) shouldBe None
      oldRecord shouldBe Some(List(firstNumber, droppedNumber))

      // a spend of the first box is not marked in the old record
      val spend = move(first, Constants.TrueTree)
      node.awaitIndexed(node.extendWith(Seq(spend)))
      node.boxRecord(first).flatMap(_.spendingTxIdOpt) shouldBe Some(spend.id)
      oldRecord shouldBe Some(List(firstNumber, droppedNumber))
      node.addressRecord shouldBe None
      node.templateRecord shouldBe None
    }
  }
}
