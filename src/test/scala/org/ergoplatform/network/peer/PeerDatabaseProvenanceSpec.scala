package org.ergoplatform.network.peer

import org.ergoplatform.network.PeerSpec
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.{
  RandomPeerExcluding,
  SeenPeers
}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.network.{Incoming, Outgoing}
import scorex.db.LDBFactory

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.net.InetSocketAddress
import java.nio.file.Files

class PeerDatabaseProvenanceSpec extends ErgoCorePropertyTest {

  import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, settings}

  property("legacy rows load only matching declared endpoints as unverified candidates") {
    val dataDir = Files.createTempDirectory("peer-database-provenance")
    val peerDir = Files.createDirectories(dataDir.resolve("peers"))
    val dbSettings = settings.copy(directory = dataDir.toString)

    val incomingAddress = new InetSocketAddress("198.51.100.10", 9030)
    val outgoingAddress = new InetSocketAddress("198.51.100.11", 9030)
    val inconsistentKey = new InetSocketAddress("198.51.100.12", 9030)
    val differentDeclaredAddress = new InetSocketAddress("198.51.100.13", 9030)
    val localOnlyKey = new InetSocketAddress("192.168.1.12", 9030)
    val claimedLocalAddress = new InetSocketAddress("192.168.1.99", 9030)

    val legacyRows = Seq(
      incomingAddress -> legacyPeer(
        Some(incomingAddress),
        claimedLocalAddress,
        Some(Incoming),
        lastHandshake = 1000L
      ),
      outgoingAddress -> legacyPeer(
        Some(outgoingAddress),
        claimedLocalAddress,
        Some(Outgoing),
        lastHandshake = 2000L
      ),
      inconsistentKey -> legacyPeer(
        Some(differentDeclaredAddress),
        claimedLocalAddress,
        Some(Incoming),
        lastHandshake = 3000L
      ),
      localOnlyKey -> legacyPeer(
        None,
        claimedLocalAddress,
        Some(Outgoing),
        lastHandshake = 4000L
      )
    )

    val store = LDBFactory.createKvDb(peerDir.toString)
    try {
      legacyRows.foreach { case (address, peerInfo) =>
        store.insert(serialize(address), PeerInfoSerializer.toBytes(peerInfo)).get
      }
    } finally store.close()

    val loaded = new PeerDatabase(dbSettings).knownPeers

    loaded(incomingAddress) shouldBe PeerInfo.fromAddress(incomingAddress)
    loaded(outgoingAddress) shouldBe PeerInfo.fromAddress(outgoingAddress)
    loaded.keySet shouldBe Set(incomingAddress, outgoingAddress)
    loaded.values.foreach { peerInfo =>
      peerInfo.peerSpec.features shouldBe empty
      peerInfo.peerSpec.address shouldBe peerInfo.peerSpec.declaredAddress
      peerInfo.lastHandshake shouldBe 0L
      peerInfo.connectionType shouldBe None
    }

    SeenPeers(10).choose(loaded, Seq.empty, null) shouldBe empty
    val dialCandidate = RandomPeerExcluding(Seq.empty).choose(loaded, Seq.empty, null)
    dialCandidate.flatMap(_.peerSpec.address).exists(loaded.contains) shouldBe true
  }

  private def legacyPeer(declaredAddress: Option[InetSocketAddress],
                         localAddress: InetSocketAddress,
                         direction: Option[scorex.core.network.ConnectionDirection],
                         lastHandshake: Long): PeerInfo = {
    val peerSpec: PeerSpec = defaultPeerSpec.copy(
      declaredAddress = declaredAddress,
      features = Seq(LocalAddressPeerFeature(localAddress))
    )
    PeerInfo(peerSpec, lastHandshake, direction)
  }

  private def serialize(obj: Object): Array[Byte] = {
    val stream = new ByteArrayOutputStream()
    val objectStream = new ObjectOutputStream(stream)
    objectStream.writeObject(obj)
    objectStream.close()
    stream.toByteArray
  }
}
