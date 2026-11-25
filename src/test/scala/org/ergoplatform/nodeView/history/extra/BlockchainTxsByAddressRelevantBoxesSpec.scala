package org.ergoplatform.nodeView.history.extra

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.handleRejections
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.http.api.BlockchainApiRoute
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.boxSegmentId
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.Stubs
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.api.http.ApiRejectionHandler
import scorex.util.{ModifierId, bytesToId}
import sigma.ast.{ErgoTree, GE, Height, IntConstant}
import sigmastate.helpers.TestingHelpers.testBox

import scala.collection.mutable.ArrayBuffer

/**
  * `transaction/byAddress` with `relevantInputs` and `relevantOutputs`: each transaction lists
  * only the inputs, the outputs or both of the queried address; everything else is as without them.
  *
  * The index rows of one address are written by hand: an airdrop-like transaction with twenty
  * outputs, two of them to the address, transactions spending those two boxes, and a transaction
  * the address lists without a box of it (as rows left by earlier versions can). Two of the boxes
  * of the address are in a box segment, the third in the row of the address. Three outputs are fee
  * outputs, which outputs listed for the address keep: one before the boxes of the address in its
  * transaction, one after them and one in the transaction without a box of the address.
  */
class BlockchainTxsByAddressRelevantBoxesSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with BeforeAndAfterAll
  with Stubs {

  private implicit val addressEncoder: ErgoAddressEncoder = settings.chainSettings.addressEncoder

  private val indexedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(extraIndex = true))
  private val route: Route = BlockchainApiRoute(digestReadersRef, indexedSettings, None).route

  private val watched: ErgoAddress =
    addressEncoder.fromProposition(ErgoTree.fromProposition(GE(Height, IntConstant(0)).toSigmaProp)).get
  private val watchedTree: ErgoTree = watched.script
  private val otherTree: ErgoTree = Constants.TrueTree
  private val feeTree: ErgoTree = settings.chainSettings.monetary.feeProposition
  private val treeHash: ModifierId = hashErgoTree(watchedTree)
  private val feeTreeHash: ModifierId = hashErgoTree(feeTree)

  private val block = chain.last.header
  private def txId(n: Int): ModifierId = bytesToId(Algos.hash("relevant boxes spec tx " + n))
  private val outsideTx: ModifierId = txId(-1)

  /** Global number of the transactions, as the address lists them. */
  private val txNumbers: Seq[Long] = Seq(100L, 101L, 102L, 103L)

  /** Inputs and outputs of each transaction, by box number; boxes 5, 17 and 22 are of the address. */
  private val txBoxes: Seq[(Seq[Long], Seq[Long])] = Seq(
    Seq(0L) -> (1L to 20L),
    Seq(5L, 21L) -> Seq(22L, 23L),
    Seq(17L) -> Seq(24L),
    Seq(25L) -> Seq(26L)
  )
  private val ownBoxes: Set[Long] = Set(5L, 17L, 22L)
  /** Fee outputs, box 3 spent. */
  private val feeBoxes: Set[Long] = Set(3L, 23L, 26L)

  /** Box number `n`, output `index` of transaction `creatingTx`. */
  private def indexedBox(n: Long, tree: ErgoTree, creatingTx: ModifierId, index: Int): IndexedErgoBox =
    new IndexedErgoBox(block.height, None, None, None,
      testBox(1000000L + n, tree, block.height, Seq.empty, Map.empty, creatingTx, index.toShort), n)

  private val boxRows: Map[Long, IndexedErgoBox] = {
    val created: Map[Long, (ModifierId, Int)] = txBoxes.zipWithIndex.flatMap { case ((_, outputs), t) =>
      outputs.zipWithIndex.map { case (n, i) => (n, (txId(t), i)) }
    }.toMap
    (0L to 26L).map { n =>
      val (creatingTx, index) = created.getOrElse(n, (outsideTx, n.toInt))
      val tree = if (ownBoxes(n)) watchedTree else if (feeBoxes(n)) feeTree else otherTree
      n -> indexedBox(n, tree, creatingTx, index)
    }.toMap
  }

  private val txRows: Seq[IndexedErgoTransaction] = txBoxes.zipWithIndex.map { case ((inputs, outputs), t) =>
    IndexedErgoTransaction(txId(t), t, block.height, 100 + t, txNumbers(t),
      inputs.toArray, outputs.toArray, Array.empty, block.id)
  }

  private val addressRow: IndexedErgoAddress = {
    val row = IndexedErgoAddress(treeHash, ArrayBuffer(txNumbers: _*), ArrayBuffer(22L))
    row.boxSegmentCount = 1
    row
  }
  private val boxSegment = IndexedErgoAddress(boxSegmentId(treeHash, 0), ArrayBuffer.empty[Long], ArrayBuffer(-5L, -17L))
  private val feeRow = IndexedErgoAddress(feeTreeHash, ArrayBuffer.empty[Long], ArrayBuffer(-3L, 23L, 26L))

  private val rows: Array[ExtraIndex] =
    (boxRows.values ++ boxRows.values.map(b => NumericBoxIndex(b.globalIndex, b.id)) ++
      txRows ++ txRows.map(tx => NumericTxIndex(tx.globalIndex, tx.id)) ++
      Seq(addressRow, boxSegment, feeRow)).toArray

  override def beforeAll(): Unit = {
    super.beforeAll()
    history.historyStorage.insertExtraTry(Array.empty, rows).get
  }

  override def afterAll(): Unit = {
    history.historyStorage.removeExtraTry(rows.map(_.id)).get
    super.afterAll()
  }

  private def query(params: String): String =
    "/blockchain/transaction/byAddress/" + addressEncoder.toString(watched) + params

  private def items(json: Json): Seq[Json] = json.hcursor.downField("items").as[Seq[Json]].toOption.get

  private def total(json: Json): Long = json.hcursor.downField("total").as[Long].toOption.get

  private def boxNumbers(tx: Json, field: String): Seq[Long] =
    tx.hcursor.downField(field).as[Seq[Json]].toOption.get.map(_.hcursor.downField("globalIndex").as[Long].toOption.get)

  /** Input and output numbers of each transaction of a response, newest first as it lists them. */
  private def listed(json: Json): Seq[(Seq[Long], Seq[Long])] =
    items(json).map(tx => boxNumbers(tx, "inputs") -> boxNumbers(tx, "outputs"))

  private def expected(relevantInputs: Boolean, relevantOutputs: Boolean): Seq[(Seq[Long], Seq[Long])] =
    txBoxes.reverse.map { case (inputs, outputs) =>
      (if (relevantInputs) inputs.filter(ownBoxes) else inputs) ->
        (if (relevantOutputs) outputs.filter(n => ownBoxes(n) || feeBoxes(n)) else outputs)
    }

  private def get(params: String): Json = Get(query(params)) ~> route ~> check {
    status shouldBe StatusCodes.OK
    responseAs[Json]
  }

  it should "list every box of every transaction by default and with both parameters false" in {
    val default = get("")
    listed(default) shouldBe expected(relevantInputs = false, relevantOutputs = false)
    total(default) shouldBe txNumbers.size
    get("?relevantInputs=false&relevantOutputs=false") shouldBe default
  }

  it should "list only the boxes of the address on the sides asked for" in {
    for (relevantInputs <- Seq(false, true); relevantOutputs <- Seq(false, true)) {
      withClue(s"relevantInputs=$relevantInputs, relevantOutputs=$relevantOutputs: ") {
        val json = get(s"?relevantInputs=$relevantInputs&relevantOutputs=$relevantOutputs")
        listed(json) shouldBe expected(relevantInputs, relevantOutputs)
        total(json) shouldBe txNumbers.size
      }
    }
  }

  it should "keep everything else of the transactions and of the boxes it lists" in {
    val default = items(get(""))
    val filtered = items(get("?relevantInputs=true&relevantOutputs=true"))
    def withoutBoxes(tx: Json): Json = tx.mapObject(_.remove("inputs").remove("outputs"))
    filtered.map(withoutBoxes) shouldBe default.map(withoutBoxes)

    // a listed box is the same as in the full transaction, with its own index
    filtered.zip(default).foreach { case (f, d) =>
      Seq("inputs", "outputs").foreach { field =>
        val all = d.hcursor.downField(field).as[Seq[Json]].toOption.get
        f.hcursor.downField(field).as[Seq[Json]].toOption.get.foreach(box => all should contain(box))
      }
    }
    val airdrop = filtered.last
    airdrop.hcursor.downField("outputs").as[Seq[Json]].toOption.get
      .map(box => box.hcursor.downField("index").as[Int].toOption.get) shouldBe Seq(2, 4, 16)
  }

  it should "keep the fee outputs among the outputs of the address, in their order" in {
    val json = get("?relevantOutputs=true")
    listed(json).map(_._2) shouldBe Seq(Seq(26L), Seq(24L).filter(ownBoxes), Seq(22L, 23L), Seq(3L, 5L, 17L))
    // fee outputs are not kept among the inputs
    listed(get("?relevantInputs=true")).flatMap(_._1).toSet shouldBe ownBoxes.intersect(txBoxes.flatMap(_._1).toSet)
  }

  it should "list only the outputs of the address when the fee address has no row" in {
    history.historyStorage.removeExtraTry(Array(feeRow.id)).get
    try {
      listed(get("?relevantOutputs=true")).map(_._2) shouldBe txBoxes.reverse.map(_._2.filter(ownBoxes))
    } finally {
      history.historyStorage.insertExtraTry(Array.empty, Array(feeRow)).get
    }
  }

  it should "page as without the parameters" in {
    val json = get("?offset=1&limit=2&relevantInputs=true&relevantOutputs=true")
    listed(json) shouldBe expected(relevantInputs = true, relevantOutputs = true).slice(1, 3)
    total(json) shouldBe txNumbers.size
  }

  it should "answer the POST route as the GET route" in {
    val params = "?relevantInputs=true&relevantOutputs=true"
    Post("/blockchain/transaction/byAddress" + params, addressEncoder.toString(watched)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json] shouldBe get(params)
    }
    Post("/blockchain/transaction/byAddress?relevantOutputs=true", addressEncoder.toString(watched)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      listed(responseAs[Json]) shouldBe expected(relevantInputs = false, relevantOutputs = true)
    }
  }

  it should "reject a value that is not a boolean, as the node rejects a malformed offset" in {
    // the rejection handler of the node, which answers a malformed query parameter with 500
    val nodeRoute = handleRejections(ApiRejectionHandler.rejectionHandler)(route)
    val malformedOffset = Get(query("?offset=abc")) ~> nodeRoute ~> check(status)
    malformedOffset should not be StatusCodes.OK
    Seq("?relevantInputs=maybe", "?relevantOutputs=maybe").foreach { params =>
      Get(query(params)) ~> nodeRoute ~> check {
        status shouldBe malformedOffset
      }
    }
  }

  it should "leave the stored transactions, shared through the database cache, with their full bodies" in {
    get("")
    val cached = history.typedExtraIndexById[IndexedErgoTransaction](txId(0)).get
    // the test is about the instance the cache shares
    history.typedExtraIndexById[IndexedErgoTransaction](txId(0)).get should be theSameInstanceAs cached
    cached.outputs.map(_.globalIndex) shouldBe (1L to 20L)

    get("?relevantInputs=true&relevantOutputs=true")
    cached.inputs.map(_.globalIndex) shouldBe Seq(0L)
    cached.outputs.map(_.globalIndex) shouldBe (1L to 20L)

    Get("/blockchain/transaction/byId/" + txId(0)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      boxNumbers(responseAs[Json], "outputs") shouldBe (1L to 20L)
    }
  }

  it should "read the row of the address afresh for every request, as the lookup of its boxes needs" in {
    // The lookup uses the row read for the request, unchanged while it is used, its box numbers
    // covering its transaction numbers. Address rows are segments, which the database cache does
    // not keep, so the indexer, which changes the rows it reads in place, does not share them.
    val row = history.typedExtraIndexById[IndexedErgoAddress](treeHash).get
    history.typedExtraIndexById[IndexedErgoAddress](treeHash).get should not be theSameInstanceAs(row)
  }
}
