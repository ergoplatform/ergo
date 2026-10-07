package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.StartExtraIndexer
import org.ergoplatform.settings.CacheSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils

import scala.concurrent.duration._

/** A supervised restart must recover the one-time history initialization. */
class ExtraIndexerRestartSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}

  private case object IsInitialized
  private case object FailOnce

  private class RestartProbeIndexer(cacheSettings: CacheSettings,
                                    addressEncoder: ErgoAddressEncoder,
                                    nodeViewHolderRef: ActorRef)
    extends ExtraIndexer(cacheSettings, addressEncoder, Some(nodeViewHolderRef), true) {
    override def receive: Receive = super.receive.orElse {
      case IsInitialized => sender() ! (_history ne null)
      case FailOnce => throw new IllegalStateException("injected actor failure")
    }
  }

  property("startup and supervised restart pull a fresh applied-state snapshot") {
    new TestKit(ActorSystem()) {
      val settings = baseSettings.copy(
        directory = createTempDir.getAbsolutePath,
        nodeSettings = baseSettings.nodeSettings.copy(extraIndex = false))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val viewHolderProbe = TestProbe()
      val indexer = system.actorOf(Props(new RestartProbeIndexer(
        settings.cacheSettings, settings.chainSettings.addressEncoder, viewHolderProbe.ref)))
      val probe = TestProbe()

      try {
        viewHolderProbe.expectMsgType[GetDataFromCurrentView[_, _]]
        viewHolderProbe.send(indexer, StartExtraIndexer(history))
        probe.send(indexer, IsInitialized)
        probe.expectMsg(true)

        probe.send(indexer, FailOnce)
        viewHolderProbe.expectMsgType[GetDataFromCurrentView[_, _]]
        viewHolderProbe.send(indexer, StartExtraIndexer(history))
        probe.awaitAssert({
          probe.send(indexer, IsInitialized)
          probe.expectMsg(1.second, true)
        }, 5.seconds, 100.millis)
      } finally {
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
      }
    }
  }
}
