package org.ergoplatform.it.api

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger
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

  private val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  server.setExecutor(serverPool)
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

  private def withNode[T](policy: WaitPolicy, problem: () => Option[String] = () => None)
                         (test: NodeApi => T): T = {
    val node = new NodeApi {
      implicit override def ec: ExecutionContext                 = NodeApiWaitSpec.this.ec
      override val restAddress: String                           = "127.0.0.1"
      override val nodeRestPort: Int                             = serverPort
      override val blockDelay: FiniteDuration                    = 1.second
      override protected val client: AsyncHttpClient             = http
      override protected def waitPolicy: WaitPolicy              = policy
      override protected def containerProblem(): Option[String] = problem()
    }
    try test(node)
    finally node.close()
  }

  private def failure(result: Future[_]): Throwable =
    Await.ready(result, 10.seconds).value.get.failed.get

  "Retrying" should "give up on a node that has not answered for the policy's limit" in {
    withNode(WaitPolicy(Some(1.second), checkContainer = false)) { node =>
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
    withNode(WaitPolicy(None, checkContainer = true), problem) { node =>
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
    withNode(WaitPolicy(Some(1.minute), checkContainer = true)) { node =>
      failure(node.get("/fail")) shouldBe an[UnexpectedStatusCodeException]
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
