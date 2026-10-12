package org.ergoplatform.it.api

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{CountDownLatch, Executors, TimeoutException}

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import org.asynchttpclient.{
  AsyncHttpClient,
  DefaultAsyncHttpClient,
  DefaultAsyncHttpClientConfig
}
import org.ergoplatform.it.api.NodeApi.{
  NodeUnreachableException,
  UnexpectedStatusCodeException,
  WaitPolicy
}
import org.ergoplatform.it.util.NoProgressException
import org.ergoplatform.settings.NetworkType
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class NodeApiWaitSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  private val released   = new CountDownLatch(1)
  private val serverPool = Executors.newCachedThreadPool() // "/hang" holds its thread
  // computed on every /info request
  @volatile private var infoBody: () => String = () => infoJson(fullHeight = 1, headers = 1)

  private val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  server.setExecutor(serverPool)
  server.createContext("/info", handler(respond(_, 200, infoBody())))
  server.createContext("/fail", handler(respond(_, 500, "failed")))
  server.createContext("/hang", handler(_ => released.await()))
  server.start()
  private val serverPort = server.getAddress.getPort

  private val http: AsyncHttpClient = new DefaultAsyncHttpClient(
    new DefaultAsyncHttpClientConfig.Builder()
      .setRequestTimeout(200)
      .setReadTimeout(200)
      .setMaxRequestRetry(0)
      .build()
  )

  override protected def afterAll(): Unit = {
    released.countDown()
    http.close()
    server.stop(0)
    serverPool.shutdownNow()
  }

  private def handler(f: HttpExchange => Unit): HttpHandler = new HttpHandler {
    override def handle(exchange: HttpExchange): Unit = f(exchange)
  }

  private def respond(exchange: HttpExchange, status: Int, body: String): Unit = {
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.sendResponseHeaders(status, bytes.length.toLong)
    exchange.getResponseBody.write(bytes)
    exchange.close()
  }

  private def infoJson(fullHeight: Int, headers: Int): String =
    s"""{"fullHeight":$fullHeight,"bestFullHeaderId":"f$fullHeight",""" +
      s""""headersHeight":$headers,"bestHeaderId":"h$headers","peersCount":0}"""

  private def withNode[T](policy: WaitPolicy,
                          problem: () => Option[String] = () => None,
                          clock: () => Deadline = () => Deadline.now)
                         (test: NodeApi => T): T = {
    val node = new NodeApi {
      implicit override def ec: ExecutionContext                 = NodeApiWaitSpec.this.ec
      override val restAddress: String                           = "127.0.0.1"
      override val nodeRestPort: Int                             = serverPort
      override val blockDelay: FiniteDuration                    = 1.second
      override protected val client: AsyncHttpClient             = http
      override protected def waitPolicy: WaitPolicy              = policy
      override protected def containerProblem(): Option[String] = problem()
      override protected def stallClock: () => Deadline          = clock
    }
    try test(node)
    finally node.close()
  }

  private def failure(result: Future[_]): Throwable =
    Await.ready(result, 10.seconds).value.get.failed.get

  "Retrying" should "give up on a node that has not answered for the policy's limit" in {
    withNode(WaitPolicy(Some(1.second), None, checkContainer = false)) { node =>
      val started = Deadline.now
      val error   = failure(node.get("/hang"))
      Deadline.now - started should be >= 1.second
      error shouldBe a[NodeUnreachableException]
      error.getMessage should (include("/hang") and include("no response for 1 second"))
    }
  }

  it should "fail on the first container check that finds the container gone" in {
    val probes = new AtomicInteger()
    val problem = () => {
      probes.incrementAndGet()
      Some("container 01-node01 status=exited exitCode=137")
    }
    withNode(WaitPolicy(None, None, checkContainer = true), problem) { node =>
      failure(node.get("/hang")).getMessage should include("exitCode=137")
      probes.get() shouldBe 1 // asked on the third attempt only
    }
  }

  it should "keep retrying an unbounded node, as it2's mainnet nodes need" in {
    withNode(WaitPolicy.Unbounded) { node =>
      val result = node.get("/hang")
      a[TimeoutException] should be thrownBy Await.ready(result, 1500.millis)
    }
  }

  it should "still fail at once on an unexpected status" in {
    withNode(WaitPolicy(Some(1.minute), None, checkContainer = true)) { node =>
      failure(node.get("/fail")) shouldBe an[UnexpectedStatusCodeException]
    }
  }

  "Height wait" should "fail once the full-block tip stands still, headers or not" in {
    val requests = new AtomicInteger()
    infoBody = () => infoJson(fullHeight = 1, headers = requests.incrementAndGet())
    withNode(WaitPolicy(None, Some(300.millis), checkContainer = false)) { node =>
      val error = failure(node.waitForHeight(5, 10.millis))
      error shouldBe a[NoProgressException]
      error.getMessage should (include("fullHeight >= 5") and include("fullHeight=1") and
        include("peersCount=0"))
    }
  }

  it should "not fail while the full-block tip moves, however long the wait" in {
    // every sample takes 200 ms of the fake clock and finds one more block
    val clock  = new AtomicReference(Deadline.now)
    val height = new AtomicInteger()
    infoBody = () => {
      clock.updateAndGet(_ + 200.millis)
      val h = height.incrementAndGet()
      infoJson(fullHeight = h, headers = h)
    }
    val policy = WaitPolicy(None, Some(300.millis), checkContainer = false)
    withNode(policy, clock = () => clock.get) { node =>
      Await.result(node.waitForHeight(10, 10.millis), 10.seconds) shouldBe 10
    }
  }

  it should "return the height once it is reached" in {
    infoBody = () => infoJson(fullHeight = 5, headers = 5)
    withNode(WaitPolicy(None, Some(300.millis), checkContainer = false)) { node =>
      Await.result(node.waitForHeight(5, 10.millis), 10.seconds) shouldBe 5
    }
  }

  it should "keep waiting on a node without a stall limit" in {
    infoBody = () => infoJson(fullHeight = 1, headers = 1)
    withNode(WaitPolicy.Unbounded) { node =>
      val result = node.waitForHeight(5, 10.millis)
      a[TimeoutException] should be thrownBy Await.ready(result, 1.second)
    }
  }

  "Wait policy" should "bound the suites' devnet nodes only" in {
    WaitPolicy.forNetwork(NetworkType.DevNet) shouldBe WaitPolicy.LocalDevNet
    WaitPolicy.forNetwork(NetworkType.DevNet60) shouldBe WaitPolicy.LocalDevNet
    WaitPolicy.forNetwork(NetworkType.MainNet) shouldBe WaitPolicy.Unbounded
    WaitPolicy.forNetwork(NetworkType.TestNet) shouldBe WaitPolicy.Unbounded
    WaitPolicy.forNetwork(NetworkType.Tests) shouldBe WaitPolicy.Unbounded
  }
}
