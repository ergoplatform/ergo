package org.ergoplatform.utils

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.settings.ErgoSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext

/** This uses TestProbe to receive messages from actor.
  * To make TestProbe work `defaultSender` implicit should be imported
  */
class NodeViewFixture(protoSettings: ErgoSettings, val testName: String = "") extends NodeViewTestContext {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("scorex.executionContext")
  implicit def ctx: NodeViewTestContext = this

  private val fileUtils = new scorex.testkit.utils.FileUtils {}
  val nodeViewDir: java.io.File = fileUtils.createTempDir
  @volatile var settings: ErgoSettings = protoSettings.copy(directory = nodeViewDir.getAbsolutePath)
  private val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider
  private val emission: EmissionRules = new EmissionRules(settings.chainSettings.monetary)
  @volatile var nodeViewHolderRef: ActorRef = ErgoNodeViewRef(settings, timeProvider, emission)
  val testProbe = new TestProbe(actorSystem)

  /** This sender should be imported to make TestProbe work! */
  implicit val defaultSender: ActorRef = testProbe.testActor

  def apply[T](test: NodeViewFixture => T): T = try test(this) finally stop()

  def startNodeViewHolder(): Unit = {
    nodeViewHolderRef = ErgoNodeViewRef(settings, timeProvider, emission)
  }

  def stopNodeViewHolder(): Unit = {
    actorSystem.stop(nodeViewHolderRef)
  }

  def updateConfig(settingsOverride: ErgoSettings => ErgoSettings): Unit = {
    stopNodeViewHolder()
    settings = settingsOverride(settings)
    startNodeViewHolder()
  }

  def stop(): Unit = {
    stopNodeViewHolder()
    actorSystem.stop(testProbe.testActor)
    actorSystem.terminate()
  }
}

object NodeViewFixture {
  def apply(protoSettings: ErgoSettings): NodeViewFixture = new NodeViewFixture(protoSettings)
}
