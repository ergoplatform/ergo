# Ergo Core library

## Establishing connections to the node

1. We need to send a Handshake message 

Create PeerSpec and all parameters (including features)

[PeerSpec](src/main/scala/org/ergoplatform/settings/PeerFeatureDescriptors.scala)'s doc:
```
* Declared information about peer
  *
  * @param agentName       - Network agent name. May contain information about client code
  *                        stack, starting from core code-base up to the end graphical interface.
  *                        Basic format is `/Name:Version(comments)/Name:Version/.../`,
  *                        e.g. `/Ergo-Scala-client:2.0.0(iPad; U; CPU OS 3_2_1)/AndroidBuild:0.8/`
  * @param protocolVersion - Identifies protocol version being used by the node
  * @param nodeName        - Custom node name
  * @param declaredAddress - Public network address of the node if any
  * @param features        - Set of node capabilities
```

All the available peer features are stored inside [PeerFeatureDescriptors](src/main/scala/org/ergoplatform/settings/PeerFeatureDescriptors.scala)


```scala
import org.ergoplatform.network.PeerSpec
import org.ergoplatform.network.Version
import java.net.InetSocketAddress

val mySpec = PeerSpec(
  agentName = "MyCoolErgoClient:0.1",
  protocolVersion = Version("version of the ergo-core library"),
  nodeName = "MyNodeName",
  declaredAddress = Some(InetSocketAddress("https://ergocoolclient.xyz", "5016")),
  features = Seq()
)
```

Then, we are ready to create our Handshake message with peer spec and UNIX time of the message
```scala
import org.ergoplatform.network.Handshake
import org.ergoplatform.network.PeerSpec
import org.ergoplatform.network.HandshakeSerializer

val handshakeMessage = Handshake(mySpec, System.currentTimeMillis())
val handshakeMessageSerialized = HandshakeSerializer.toBytes(handshakeMessage)
```
Now we can serialize the message and send it
If the message arrived successfully, we'll receive Handshake message back, so we can start to exchange messages with the node

2. Now we can start syncing with the node

```
/**
  * @param lastHeaders - some recent headers (including last one) known to a peer
  */
```
Send [ErgoSyncInfoV2](src/main/scala/org/ergoplatform/nodeView/history/ErgoSyncInfo.scala) with empty `lastHeaders`, so the node knows we're just beginning to sync
```scala
import org.ergoplatform.nodeView.history.ErgoSyncInfoV2
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec

val syncMessage = ErgoSyncInfoV2(Seq())
val syncMessageSerialized = ErgoSyncInfoMessageSpec.toBytes(syncMessage)
```
The client will start receiving [InvData](src/main/scala/org/ergoplatform/network/message/InvData.scala) messages with 
```
/**
  * P2P network message which is encoding "inventory", transactions or block sections the node has
  *
  * @param typeId
  * @param ids
  */
```
```scala
import org.ergoplatform.network.message.InvSpec

{ invDataMessage =>
  val invData = InvSpec.parseBytesTry(invDataMessage)
}
```
