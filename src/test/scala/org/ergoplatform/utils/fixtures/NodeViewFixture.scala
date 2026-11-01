package org.ergoplatform.utils.fixtures

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.utils.NodeViewTestContext
import org.ergoplatform.wallet.utils.TestFileUtils

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/** This uses TestProbe to receive messages from actor.
  * To make TestProbe work `defaultSender` implicit should be imported
  */
class NodeViewFixture(protoSettings: ErgoSettings, parameters: Parameters) extends NodeViewTestContext with TestFileUtils { self =>

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("scorex.executionContext")
  implicit def ctx: NodeViewTestContext = this

  val nodeViewDir: java.io.File = createTempDir
  @volatile var settings: ErgoSettings = protoSettings.copy(directory = nodeViewDir.getAbsolutePath)
  val emission: EmissionRules = new EmissionRules(settings.chainSettings.monetary)
  @volatile var nodeViewHolderRef: ActorRef = ErgoNodeViewRef(settings)
  val testProbe = new TestProbe(actorSystem)

  /** This sender should be imported to make TestProbe work! */
  implicit val defaultSender: ActorRef = testProbe.testActor

  def apply[T](test: self.type => T): T = try test(self) finally stop()

  def startNodeViewHolder(): Unit = {
    nodeViewHolderRef = ErgoNodeViewRef(settings)
  }

  /** Returns once the holder has stopped. Its postStop closes history and state,
    * so a restart or a test can open the same directory.
    */
  def stopNodeViewHolder(): Unit = {
    val holder  = nodeViewHolderRef
    val watcher = new TestProbe(actorSystem)
    watcher.watch(holder)
    actorSystem.stop(holder)
    watcher.expectTerminated(holder, 30.seconds)
    actorSystem.stop(watcher.ref)
  }

  /** Restarts nodeViewHolder and applies config override */
  def updateConfig(settingsOverride: ErgoSettings => ErgoSettings): Unit = {
    stopNodeViewHolder()
    settings = settingsOverride(settings)
    startNodeViewHolder()
  }

  def stop(): Unit =
    try stopNodeViewHolder()
    finally {
      actorSystem.stop(testProbe.testActor)
      actorSystem.terminate()
    }
}

object NodeViewFixture {
  def apply(protoSettings: ErgoSettings, parameters: Parameters): NodeViewFixture =
    new NodeViewFixture(protoSettings, parameters)
}
