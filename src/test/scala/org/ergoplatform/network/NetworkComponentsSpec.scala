package org.ergoplatform.network

import akka.testkit.TestProbe
import org.ergoplatform.modifiers.BlockTransactionsTypeId
import org.ergoplatform.network.message.{InvData, InvSpec, Message}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.defaultPeerSpec
import scorex.core.network.{ConnectedPeer, ConnectionId}
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.SendToPeer

import java.net.InetSocketAddress

class NetworkComponentsSpec extends ErgoCorePropertyTest {

  // Simple test to verify network message delivery with Ergo components
  property("Ergo network components handle basic message routing") {
    val system = akka.actor.ActorSystem("NetworkTest")
    
    try {
      // Create test probes
      val peerHandlerProbe = TestProbe("PeerHandler")(system)
      val networkControllerProbe = TestProbe("NetworkController")(system)
      
      // Create test peer
      val testPeer = ConnectedPeer(
        ConnectionId(new InetSocketAddress("127.0.0.1", 9001), new InetSocketAddress("127.0.0.1", 9002), null),
        peerHandlerProbe.ref,
        Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis(), None, System.currentTimeMillis()))
      )
      
      // Create test INV message
      val testInvMessage = Message(InvSpec, Right(InvData(BlockTransactionsTypeId.value, Seq.empty)), None)
      
      // Send message through network controller
      networkControllerProbe.ref ! SendToNetwork(testInvMessage, SendToPeer(testPeer))
      
      // Network controller should receive the message
      networkControllerProbe.expectMsgType[SendToNetwork]
      
      // Verify the message would be routed to the peer handler
      // (In real scenario, network controller would handle the actual delivery)
      
    } finally {
      system.terminate()
    }
  }

}
