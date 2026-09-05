package org.ergoplatform.it

import com.typesafe.config.Config
import io.circe.{Decoder, Json}
import io.circe.parser.parse
import io.circe.syntax._
import org.asynchttpclient.Response
import org.ergoplatform.ErgoBox
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.it.container.{Docker, IntegrationTestConstants, Node}
import org.ergoplatform.mining.AutolykosSolutionJsonCodecs
import org.ergoplatform.mining.llm_generated.MatrixTestMiner
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceImpl
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.{ErgoValidationSettingsUpdate, NetworkType, Parameters}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base16
import sigma.serialization.GroupElementSerializer

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/** Normal, valid Matrix traffic on a private devnet with a shared custom genesis. */
class MatrixLifecycleSpec extends AnyFlatSpec
  with Matchers with BeforeAndAfterAll with IntegrationTestConstants with ApiCodecs {

  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fee = 1000000L
  private val operationTimeout = 30.seconds
  private val convergenceTimeout = 120.seconds
  private var runtime: Docker = _
  private var nodeList = Vector.empty[Node]
  private var configs = Vector.empty[Config]
  private var dataPaths = Vector.empty[String]
  private var fixtureRoot: Path = _
  private var miningKeys = Map.empty[String, BigInt]
  private var retainedTransaction: Option[ErgoTransaction] = None
  private var normalScenarioComplete = false
  private var forkScenarioComplete = false

  private def await[A](f: Future[A]): A = Await.result(f, operationTimeout)

  private def body(response: Response): Json = {
    withClue(s"HTTP ${response.getStatusCode}: ${response.getResponseBody}") {
      response.getStatusCode shouldBe 200
    }
    parse(response.getResponseBody).fold(throw _, identity)
  }

  private def get(node: Node, path: String): Json =
    withClue(s"${node.nodeName} GET $path: ") {
      body(await(node.singleGet(path, _.setHeader("api_key", "hello"))))
    }

  private def post(node: Node, path: String, value: Json): Json =
    withClue(s"${node.nodeName} POST $path: ") {
      body(await(node.post(path, value.noSpaces)))
    }

  private def field[A: Decoder](json: Json, name: String): A =
    json.hcursor.get[A](name).fold(throw _, identity)

  private def until[A](label: String)(observe: => A)(accept: A => Boolean): A = {
    val deadline = convergenceTimeout.fromNow
    var observed = observe
    while (!accept(observed) && deadline.hasTimeLeft()) {
      Thread.sleep(200)
      observed = observe
    }
    withClue(s"$label; last observation: $observed") { accept(observed) shouldBe true }
    observed
  }

  private def height(node: Node): Int =
    field[Option[Int]](get(node, "/info"), "fullHeight").getOrElse(0)

  private def inputTip(node: Node): String =
    field[String](get(node, "/blocks/bestInputBlock"), "bestInputBlock")

  private def mempool(node: Node): Set[String] =
    get(node, "/transactions/unconfirmed?limit=100").asArray.get.map(field[String](_, "id")).toSet

  private def walletBoxes(node: Node): Vector[ErgoBox] =
    get(node, "/wallet/boxes/unspent?minConfirmations=-1&limit=1000").asArray.get
      .map(j => j.hcursor.downField("box").as[ErgoBox].fold(throw _, identity))

  private def walletIds(node: Node): Set[String] = walletBoxes(node).map(b => Base16.encode(b.id)).toSet

  private def walletBalance(node: Node): Long =
    field[Long](get(node, "/wallet/balances/withUnconfirmed"), "balance")

  private def currentParameters(node: Node): Parameters = {
    val info = get(node, "/info")
    val p = info.hcursor.downField("parameters")
    val version = p.get[Int]("blockVersion").fold(throw _, identity)
    val subblocks = p.get[Int]("subblocksPerBlock").fold(throw _, identity)
    Parameters(height(node), Parameters.DefaultParameters
      .updated(Parameters.BlockVersion, version)
      .updated(Parameters.SubblocksPerBlockIncrease, subblocks), ErgoValidationSettingsUpdate.empty)
  }

  private def mine(node: Node, input: Boolean, txs: Seq[ErgoTransaction] = Seq.empty): String = {
    val previousHeight = height(node)
    val previousInput = inputTip(node)
    val work = if (txs.nonEmpty) post(node, "/mining/candidateWithTxs", txs.asJson)
      else get(node, "/mining/candidate")
    val miningSecret = miningKeys.getOrElse(field[String](work, "pk"),
      fail("Mining work public key does not belong to the fixture wallet"))
    val solution = MatrixTestMiner.solve(work, node.settings.chainSettings.powScheme,
      miningSecret, currentParameters(node), input)
    if (input) {
      post(node, "/mining/weakSolution", Json.obj(
        "pk" -> field[Json](work, "pk"), "n" -> Base16.encode(solution.n).asJson))
      until(s"${node.nodeName}: input block applied")(inputTip(node))(
        id => id.nonEmpty && id != previousInput)
    } else {
      post(node, "/mining/solution", AutolykosSolutionJsonCodecs.jsonEncoder(solution))
      until(s"${node.nodeName}: ordering block applied")(height(node))(_ == previousHeight + 1)
      field[String](get(node, "/info"), "bestFullHeaderId")
    }
  }

  private def settled(): (String, String) = {
    val result = until("three nodes agree on a present best block and state root") {
      nodeList.map { node =>
        val info = get(node, "/info")
        (field[Option[String]](info, "bestFullHeaderId").getOrElse(""),
          field[Option[String]](info, "stateRoot").getOrElse(""))
      }
    }(values => values.forall(v => v._1.nonEmpty && v._2.nonEmpty) && values.distinct.size == 1)
    val expectedHeight = height(nodeList.head)
    until("wallet scans reach the ordering checkpoint") {
      nodeList.map(n => field[Int](get(n, "/wallet/status"), "walletHeight"))
    }(_.forall(_ == expectedHeight))
    // Input-block broadcast selects direct peers whose advertised height is
    // within two blocks. Rapid devnet mining can outrun the sync status timer.
    until("each node knows both peers at the current ordering checkpoint") {
      nodeList.map(n => get(n, "/peers/syncInfo").asArray.get.map(field[Int](_, "height")))
    }(_.forall(hs => hs.size >= 2 && hs.forall(h => math.abs(h - expectedHeight) <= 2)))
    result.head
  }

  private def spendable(node: Node): Vector[ErgoBox] = {
    val h = height(node)
    walletBoxes(node).filter(b => b.creationHeight + 1 <= h && b.value > 10000000L)
      .sortBy(b => (b.creationHeight, Base16.encode(b.id)))
  }

  private def payment(node: Node, input: ErgoBox, amount: Long): ErgoTransaction = {
    val address = get(node, "/wallet/addresses").asArray.get.head.asString.get
    val request = Json.obj(
      "requests" -> Json.arr(Json.obj("address" -> address.asJson, "value" -> amount.asJson)),
      "fee" -> fee.asJson,
      "inputsRaw" -> Json.arr(Base16.encode(ErgoBoxSerializer.toBytes(input)).asJson))
    post(node, "/wallet/transaction/generate", request).as[ErgoTransaction].fold(throw _, identity)
  }

  private def submit(node: Node, tx: ErgoTransaction): Unit = {
    post(node, "/transactions", tx.asJson).asString.get shouldBe tx.id
  }

  private def connectPeer(node: Node, peer: Node): Unit = {
    post(node, "/peers/connect",
      s"${peer.nodeInfo.networkIpAddress}:${peer.nodeInfo.containerNetworkPort}".asJson)
  }

  private def reconnect(node: Node): Unit = {
    runtime.connectToNetwork(node)
    nodeList.filterNot(_ == node).foreach(connectPeer(node, _))
  }

  private def assertAppliedInput(id: String, txs: Seq[ErgoTransaction], nodes: Seq[Node]): Unit = {
    nodes.foreach { node =>
      until(s"${node.nodeName}: selected input tip")(inputTip(node))(_ == id)
      val included = get(node, s"/blocks/$id/inputBlockTransactionIds").as[Seq[String]].fold(throw _, identity)
      txs.foreach { tx =>
        included should contain(tx.id)
        until(s"${node.nodeName}: included transaction leaves mempool")(mempool(node))(!_.contains(tx.id))
        until(s"${node.nodeName}: input-block wallet transition")(walletIds(node)) { ids =>
          ids.contains(Base16.encode(tx.outputs.head.id)) &&
            tx.inputs.forall(in => !ids.contains(Base16.encode(in.boxId)))
        }
      }
    }
  }

  private def assertConfirmed(orderingId: String, txs: Seq[ErgoTransaction]): Unit = {
    nodeList.foreach { node =>
      val section = get(node, s"/blocks/$orderingId/transactions")
      field[String](section, "headerId") shouldBe orderingId
      val ids = field[Vector[Json]](section, "transactions").map(field[String](_, "id"))
      txs.foreach { tx =>
        ids should contain(tx.id)
        val outputId = Base16.encode(tx.outputs.head.id)
        val confirmed = get(node, s"/utxo/byId/$outputId").as[ErgoBox].fold(throw _, identity)
        Base16.encode(confirmed.id) shouldBe outputId
      }
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val tempRoot = Option(System.getenv("TMPDIR")).filter(_.nonEmpty)
      .map(Paths.get(_)).getOrElse(Paths.get(System.getProperty("java.io.tmpdir")))
    Files.createDirectories(tempRoot)
    fixtureRoot = Files.createTempDirectory(tempRoot, "ergo-matrix-lifecycle-")
    runtime = new Docker(tag = getClass.getSimpleName,
      localDataVolumeOpt = Some(fixtureRoot.toString), publishedPortHost = "127.0.0.1",
      nodeImage = sys.env.getOrElse("ERGO_MATRIX_TEST_IMAGE", Docker.ErgoImageLatest),
      useConfigFile = true)
    val common = MatrixDevnetConfig(defaultConfigTemplate(NetworkType.DevNet).withFallback(allowLocalConfig))
    configs = nodeSeedConfigs.take(3).map { c =>
      specialDataDirConfig("/app").withFallback(c).withFallback(common).resolve()
    }.toVector
    dataPaths = configs.indices.map { i =>
      val dataPath = Files.createDirectories(fixtureRoot.resolve(s"node-${i + 1}"))
      Files.createDirectories(dataPath.resolve("wallet").resolve("keystore"))
      dataPath.toString
    }.toVector
    configs.indices.foreach { i =>
      val node = runtime.startDevNetNode(configs(i), specialVolumeOpt = Some(dataPaths(i) -> "/app")).get
      nodeList :+= node
      await(node.waitForStartup)
    }
    connectPeer(nodeList(1), nodeList(2))
    nodeList.foreach(n => await(n.waitForPeers(2)))
    val parameters = Parameters(0, Parameters.DefaultParameters, ErgoValidationSettingsUpdate.empty)
    val mnemonic = SecretString.create(configs.head.getString("ergo.wallet.testMnemonic"))
    miningKeys = new ErgoWalletServiceImpl(nodeList.head.settings)
      .buildProverFromMnemonic(mnemonic, Some(configs.head.getInt("ergo.wallet.testKeysQty")), parameters)
      .hdKeys.map { key =>
        Base16.encode(GroupElementSerializer.toBytes(key.publicKey.key.value)) -> BigInt(key.privateInput.w)
      }.toMap
    while (currentParameters(nodeList.head).blockVersion < 4 && height(nodeList.head) < 96) {
      mine(nodeList.head, input = false)
    }
    currentParameters(nodeList.head).blockVersion shouldBe 4
    mine(nodeList.head, input = false)
    settled()
    nodeList.foreach(n => spendable(n) should not be empty)
  }

  "Matrix on three nodes" should "apply locally and remotely produced input blocks to wallet and mempool" in {
    for (producer <- nodeList.take(2)) {
      val source = spendable(producer).head
      val balance = walletBalance(producer)
      val tx = payment(producer, source, 2000000L)
      val blockOnlyReceiver = if (producer == nodeList.head) Some(nodeList.last) else None
      var disconnected = false
      try {
        blockOnlyReceiver.foreach { receiver =>
          runtime.disconnectFromNetwork(receiver)
          disconnected = true
        }
        submit(producer, tx)
        val connected = nodeList.filterNot(n => blockOnlyReceiver.contains(n))
        connected.foreach(n => until("transaction propagated before mining")(mempool(n))(_.contains(tx.id)))
        val id = mine(producer, input = true, txs = Seq(tx))
        assertAppliedInput(id, Seq(tx), connected)
        blockOnlyReceiver.foreach { receiver =>
          mempool(receiver) should not contain tx.id
          walletIds(receiver) should contain(Base16.encode(source.id))
          walletIds(receiver) should not contain Base16.encode(tx.outputs.head.id)
          reconnect(receiver)
          disconnected = false
          assertAppliedInput(id, Seq(tx), Seq(receiver))
        }
        nodeList.foreach(n => walletBalance(n) shouldBe balance - fee)
        val orderingId = mine(producer, input = false)
        settled()
        assertConfirmed(orderingId, Seq(tx))
        nodeList.map(walletBalance).distinct.size shouldBe 1
      } finally {
        if (disconnected) blockOnlyReceiver.foreach(runtime.connectToNetwork)
      }
    }
    normalScenarioComplete = true
  }

  it should "reconcile wallet and eligible mempool transactions after an input-chain fork" in {
    assume(normalScenarioComplete, "The preceding normal-traffic scenario must pass")
    settled()
    val isolated = nodeList.head
    val winner = nodeList(1)
    val inputs = spendable(isolated).take(2)
    inputs.size shouldBe 2
    val abandoned = payment(isolated, inputs.head, 2000000L)
    val retained = payment(isolated, inputs(1), 4000000L)
    retainedTransaction = Some(retained)
    val selected = payment(winner, inputs.head, 3000000L)
    abandoned.id should not be selected.id
    val previousTip = inputTip(winner)
    runtime.disconnectFromNetwork(isolated)
    var disconnected = true
    try {
      // Docker disconnects the suite bridge synchronously. A node's TCP peer
      // registry can lag that transition; prove isolation through distinct tips.
      submit(isolated, abandoned)
      submit(isolated, retained)
      val abandonedTip = mine(isolated, input = true, txs = Seq(abandoned, retained))
      assertAppliedInput(abandonedTip, Seq(abandoned, retained), Seq(isolated))
      inputTip(winner) shouldBe previousTip
      submit(winner, selected)
      val first = mine(winner, input = true, txs = Seq(selected))
      assertAppliedInput(first, Seq(selected), nodeList.tail)
      val disconnectedTip = mine(winner, input = true)
      disconnectedTip should not be abandonedTip
      inputTip(isolated) shouldBe abandonedTip
      reconnect(isolated)
      disconnected = false
      settled()
      // Resume ordinary mining after reconnection. Header sync does not carry
      // input tips, so a new announcement triggers missing-parent retrieval.
      val selectedTip = mine(winner, input = true)
      nodeList.foreach(n => until("longer input chain selected")(inputTip(n))(_ == selectedTip))
      until("eligible abandoned transaction restored to producer mempool")(mempool(isolated))(_.contains(retained.id))
      nodeList.foreach { n =>
        until("abandoned payment absent from wallet")(walletIds(n))(!_.contains(Base16.encode(abandoned.outputs.head.id)))
        walletIds(n) should contain(Base16.encode(selected.outputs.head.id))
        until("conflicting abandoned transaction removed during fork reconciliation")(mempool(n))(!_.contains(abandoned.id))
      }
      val orderingId = mine(winner, input = false)
      settled()
      assertConfirmed(orderingId, Seq(selected))
      nodeList.foreach { n =>
        until("conflicting abandoned transaction absent")(mempool(n))(!_.contains(abandoned.id))
      }
      forkScenarioComplete = true
    } finally {
      if (disconnected) runtime.connectToNetwork(isolated)
    }
  }

  it should "recover the same settled wallet and state after restarting a node" in {
    assume(forkScenarioComplete, "The preceding fork scenario must pass")
    retainedTransaction.foreach { tx =>
      val id = mine(nodeList.head, input = true, txs = Seq(tx))
      assertAppliedInput(id, Seq(tx), nodeList)
      val orderingId = mine(nodeList.head, input = false)
      settled()
      assertConfirmed(orderingId, Seq(tx))
    }
    val checkpoint = settled()
    nodeList.foreach { n =>
      until("settled mempool before restart")(mempool(n))(_.isEmpty)
      inputTip(n) shouldBe empty
    }
    val oldNode = nodeList(2)
    val boxes = walletIds(oldNode)
    boxes should not be empty
    val balance = walletBalance(oldNode)
    runtime.stopAndRemoveNode(oldNode)
    val restarted = runtime.startDevNetNode(configs(2), specialVolumeOpt = Some(dataPaths(2) -> "/app")).get
    nodeList = nodeList.updated(2, restarted)
    await(restarted.waitForStartup)
    connectPeer(restarted, nodeList(1))
    settled() shouldBe checkpoint
    walletIds(restarted) shouldBe boxes
    walletBalance(restarted) shouldBe balance
    mine(nodeList.head, input = false)
    settled()
  }

  override protected def afterAll(): Unit = {
    try {
      if (runtime != null) runtime.close()
    } finally {
      miningKeys = Map.empty
      super.afterAll()
    }
  }
}
