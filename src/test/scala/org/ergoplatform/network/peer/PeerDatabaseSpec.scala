package org.ergoplatform.network.peer

import java.net.InetSocketAddress
import java.nio.file.Files

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants._

import scala.concurrent.duration._

class PeerDatabaseSpec extends ErgoCorePropertyTest {

  private def freshPeerDatabase: PeerDatabase = {
    val dir            = Files.createTempDirectory("peer-db-spec").toString
    val peerDbSettings = settings.copy(
      directory = dir,
      scorexSettings = settings.scorexSettings.copy(
        network = settings.scorexSettings.network.copy(
          penaltySafeInterval = 500.millis,
          penaltyScoreThreshold = 30
        )
      )
    )
    new PeerDatabase(peerDbSettings)
  }

  property("penalty score should not accumulate within safe interval") {
    val db      = freshPeerDatabase
    val address = new InetSocketAddress("192.0.2.2", 9030)

    val firstApplied = db.penalize(address, PenaltyType.SpamPenalty)
    firstApplied shouldBe false
    db.penaltyScore(address) shouldBe PenaltyType.SpamPenalty.penaltyScore

    val secondApplied = db.penalize(address, PenaltyType.SpamPenalty)
    secondApplied shouldBe false
    db.penaltyScore(address) shouldBe PenaltyType.SpamPenalty.penaltyScore
  }

  property("ban threshold should be reached despite continuous penalties within safe interval") {
    val db       = freshPeerDatabase
    val address  = new InetSocketAddress("192.0.2.1", 9030)
    val deadline = System.currentTimeMillis() + 10000
    var banned   = false
    while (!banned && System.currentTimeMillis() < deadline) {
      banned = db.penalize(address, PenaltyType.SpamPenalty)
      Thread.sleep(5)
    }
    banned shouldBe true
  }

}
