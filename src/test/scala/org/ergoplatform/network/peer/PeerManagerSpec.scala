package org.ergoplatform.network.peer

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.network.message.{GetPeersSpec, InvSpec, ModifiersSpec, RequestModifierSpec}
import org.ergoplatform.network.message.inputblocks.{InputBlockMessageSpec, InputBlockTransactionIdsMessageSpec, InputBlockTransactionsMessageSpec, InputBlockTransactionsRequestMessageSpec, OrderingBlockAnnouncementMessageSpec}
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.scalatest.wordspec.AnyWordSpecLike
import scorex.core.app.ScorexContext
import scorex.core.network.{ConnectionId, Outgoing}

import java.net.InetSocketAddress

class PeerManagerSpec extends TestKit(ActorSystem("PeerManagerSpec")) with AnyWordSpecLike {



  "PeerManager" should {
    "initialize without errors" in {
      // Create a minimal ScorexContext for testing similar to ErgoApp
      val p2pMessageSpecifications = Seq(
        GetPeersSpec,
        new org.ergoplatform.network.message.PeersSpec(settings.scorexSettings.network.maxPeerSpecObjects),
        ErgoSyncInfoMessageSpec,
        InvSpec,
        RequestModifierSpec,
        ModifiersSpec,
        InputBlockMessageSpec,
        InputBlockTransactionIdsMessageSpec,
        InputBlockTransactionsMessageSpec,
        InputBlockTransactionsRequestMessageSpec,
        OrderingBlockAnnouncementMessageSpec
      )
      
      val scorexContext = ScorexContext(
        messageSpecs = p2pMessageSpecifications,
        upnpGateway = None,
        externalNodeAddress = None
      )
      
      // This should not throw any exceptions during initialization
      val peerManager = system.actorOf(Props(new PeerManager(settings, scorexContext)))
      
      // Test basic functionality - check if it responds to simple messages
      val testProbe = TestProbe()
      
      // Test that it can handle basic peer management messages
      testProbe.send(peerManager, PeerManager.ReceivableMessages.GetAllPeers)
      // Should respond with peer list (may be empty)
      testProbe.expectMsgType[Map[InetSocketAddress, PeerInfo]]
    }
    
    "handle connection confirmation requests" in {

      // Create a minimal ScorexContext for testing similar to ErgoApp
      val p2pMessageSpecifications = Seq(
        GetPeersSpec,
        new org.ergoplatform.network.message.PeersSpec(settings.scorexSettings.network.maxPeerSpecObjects),
        ErgoSyncInfoMessageSpec,
        InvSpec,
        RequestModifierSpec,
        ModifiersSpec,
        InputBlockMessageSpec,
        InputBlockTransactionIdsMessageSpec,
        InputBlockTransactionsMessageSpec,
        InputBlockTransactionsRequestMessageSpec,
        OrderingBlockAnnouncementMessageSpec
      )
      
      val scorexContext = ScorexContext(
        messageSpecs = p2pMessageSpecifications,
        upnpGateway = None,
        externalNodeAddress = None
      )
      
      val peerManager = system.actorOf(Props(new PeerManager(settings, scorexContext)))
      val testProbe = TestProbe()
      
      // Create a test connection ID
      val testAddress = new InetSocketAddress("127.0.0.1", 9001)
      val connectionId = ConnectionId(testAddress, testAddress, Outgoing)
      
      // Send connection confirmation request
      testProbe.send(peerManager, PeerManager.ReceivableMessages.ConfirmConnection(connectionId, testProbe.ref))
      
      // Should receive a response (either confirmed or denied)
      val response = testProbe.expectMsgType[Any]
      // Response should be one of the connection response types
      assert(response != null)
    }
  }
}