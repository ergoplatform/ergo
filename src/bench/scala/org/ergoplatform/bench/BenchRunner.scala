package org.ergoplatform.bench

import java.io.{File, FileInputStream, FileWriter}
import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import org.ergoplatform.bench.misc.ModifierWriter
import org.ergoplatform.bench.protocol.{Start, SubTo}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.settings.{ChainSettings, ErgoSettings}
import scorex.core.NodeViewHolder.EventType
import scorex.core.network.NodeViewSynchronizer.ModifiersFromRemote
import scorex.core.network.{ConnectedPeer, Handshake, Incoming}
import scorex.core.utils.{NetworkTimeProvider, NetworkTimeProviderSettings, ScorexLogging}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.io.Source

object BenchRunner extends ScorexLogging {

  implicit val system: ActorSystem = ActorSystem("bench")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val targetDirectory = "target/bench"

  def main(args: Array[String]): Unit = {

    new File(targetDirectory).mkdirs()

    val threshold = args.headOption.getOrElse("1000").toInt

    val benchRef = BenchActor(threshold)
    val userDir = TempDir.createTempDir

    log.info(s"User dir is $userDir")
    log.info("Starting benchmark.")

    lazy val ergoSettings: ErgoSettings = ErgoSettings.read(None).copy(
      directory =  userDir.getAbsolutePath,
      chainSettings = ChainSettings(1 minute, 1, 100, FakePowForBench)
    )

    log.info(s"Setting that being used:")
    log.info(s"$ergoSettings")


    val ntpSettings = NetworkTimeProviderSettings("pool.ntp.org", 30 minutes, 30 seconds)
    val timeProvider = new NetworkTimeProvider(ntpSettings)

    val peer = ConnectedPeer(
      new InetSocketAddress("localhost", 9001),
      benchRef,
      Incoming,
      Handshake("hey", scorex.core.app.Version.apply("1.0.0"), "assault", None, System.currentTimeMillis())
    )

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

    log.info("Starting to read modifiers.")
    val modifiers = readModifiers(peer)
    log.info("Finished read modifiers, starting to bench.")
    log.info(s"$threshold modifiers to go")
    runBench(benchRef, nodeViewHolderRef, modifiers)
    //writeHtml
  }

  private def readModifiers(peer: ConnectedPeer): Vector[ModifiersFromRemote] = {
    var counter = 0
    log.info("Start reading modifiers from data file.")
    val is = getClass.getResourceAsStream("/bench/mods.dat")
    val result = Stream
      .continually {
        val mod = ModifierWriter.read(is)
        mod.map { case (id, arr) =>
          counter += 1
          if (counter % 1000 == 0) {
            log.info(s"Already read $counter modifiers.")
          }
          ModifiersFromRemote(peer, id, Seq(arr))

        }
      }
      .takeWhile(_.isDefined)
      .flatten
      .toVector

    result
  }

  private def runBench(benchRef: ActorRef, nodeRef: ActorRef, modifiers: Vector[ModifiersFromRemote]): Unit = {
    benchRef ! SubTo(nodeRef, Seq(EventType.SuccessfulSyntacticallyValidModifier))
    benchRef ! Start
    modifiers.foreach { m => nodeRef ! m }
  }


  private def writeHtml: Unit = {
    val html = Source.fromInputStream(getClass.getResourceAsStream("/bench/chart.html")).getLines().mkString("\n")
    val js = Source.fromInputStream(getClass.getResourceAsStream("/bench/chart.js")).getLines().mkString("\n")
    val writer1 = new FileWriter("target/bench/chart.html")
    writer1.write(html)
    writer1.flush()
    writer1.close()
    val writer2 = new FileWriter("target/bench/chart.js")
    writer2.write(js)
    writer2.flush()
    writer2.close()
  }

}
