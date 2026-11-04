package org.ergoplatform.utils.fixtures

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.utils.NodeViewTestContext
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.db.{LDBFactory, StoreRegistry}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.Try

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

  def stopNodeViewHolder(): Unit = {
    if (!actorSystem.whenTerminated.isCompleted) {
      val observer = TestProbe()(actorSystem)
      observer.watch(nodeViewHolderRef)
      actorSystem.stop(nodeViewHolderRef)
      observer.expectTerminated(nodeViewHolderRef, 20.seconds)
      actorSystem.stop(observer.ref)
    }
  }

  /** Restarts nodeViewHolder and applies config override */
  def updateConfig(settingsOverride: ErgoSettings => ErgoSettings): Unit = {
    stopNodeViewHolder()
    settings = settingsOverride(settings)
    startNodeViewHolder()
  }

  def stop(): Unit = {
    try stopNodeViewHolder() finally {
      Await.result(actorSystem.terminate(), 30.seconds)
      // Wallet stores are not closed by actor shutdown. Release only this fixture's
      // stores after all actors have terminated; other suites may still be running.
      val registry = LDBFactory.factory.asInstanceOf[StoreRegistry]
      registry.lock.writeLock().lock()
      try {
        val owned = registry.map.toVector.collect {
          case (path, store) if path.toPath.toAbsolutePath.normalize()
            .startsWith(nodeViewDir.toPath.toAbsolutePath.normalize()) => store
        }
        val closed = owned.map(store => Try(store.close()))
        closed.foreach(_.get)
      } finally registry.lock.writeLock().unlock()
    }
  }
}

object NodeViewFixture {
  def apply(protoSettings: ErgoSettings, parameters: Parameters): NodeViewFixture =
    new NodeViewFixture(protoSettings, parameters)
}
