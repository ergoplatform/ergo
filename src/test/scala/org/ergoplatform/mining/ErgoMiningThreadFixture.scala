package org.ergoplatform.mining

import akka.actor.ActorSystem
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import com.google.common.primitives.Ints
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoCoreTestConstants.defaultMinerSecret
import scorex.crypto.hash.Blake2b256

import scala.concurrent.duration._

/** Settings and a real candidate for the miner thread specs. */
object ErgoMiningThreadFixture {

  // the periodic poll fires 1 s after start and then once an hour, so in a test every later candidate is sent by hand
  val settings: ErgoSettings = {
    val empty = ErgoSettingsReader.read()
    empty.copy(
      nodeSettings = empty.nodeSettings.copy(
        mining = true,
        stateType = StateType.Utxo,
        internalMinerPollingInterval = 1.hour,
        offlineGeneration = true,
        verifyTransactions = true
      ),
      chainSettings = empty.chainSettings.copy(blockInterval = 1.seconds)
    )
  }

  /** A real candidate, built by a real generator on a chain of its own. A `Candidate` is plain data, so the actor
    * system that built it is shut down before it is returned. */
  def realCandidate(name: String): Candidate = {
    implicit val system: ActorSystem = ActorSystem(s"candidate-$name")
    try {
      val chainSettings = settings.copy(directory = s"${settings.directory}-$name-${System.nanoTime()}")
      val viewHolderRef = ErgoNodeViewRef(chainSettings)
      val readersHolderRef = ErgoReadersHolderRef(viewHolderRef)
      val realGenerator =
        CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, chainSettings)
      val candidateProbe = TestProbe()
      realGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), candidateProbe.ref)
      // the first request waits for the genesis state, which can take a while on a loaded machine
      candidateProbe.expectMsgPF(60.seconds) { case StatusReply.Success(c: Candidate) => c }
    } finally {
      TestKit.shutdownActorSystem(system)
    }
  }

  /** `c` with its timestamp moved by `k` ms and a PoW message of its own, so it reads as new work either way. */
  def shifted(c: Candidate, k: Int): Candidate =
    c.copy(
      candidateBlock = c.candidateBlock.copy(timestamp = c.candidateBlock.timestamp + k),
      externalVersion = c.externalVersion.copy(msg = Blake2b256(c.externalVersion.msg ++ Ints.toByteArray(k)))
    )
}
