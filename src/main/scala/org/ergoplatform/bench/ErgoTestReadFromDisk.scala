package org.ergoplatform.bench

import java.io.{File, FileWriter}
import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.bench.BenchActor.{Start, Sub}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.settings.ErgoSettings
import scorex.core.ModifierTypeId
import scorex.core.network.NodeViewSynchronizer.ModifiersFromRemote
import scorex.core.network.{ConnectedPeer, Handshake, Incoming}
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

object ErgoTestReadFromDisk extends ScorexLogging {

  implicit val system: ActorSystem = ActorSystem("bench")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val targetDirectory = "target/bench"

  def main(args: Array[String]): Unit = {

    new File(targetDirectory).mkdirs()

    val threshold =args.headOption.getOrElse("10000").toInt

    val benchRef = system.actorOf(Props.apply(classOf[BenchActor], threshold))
    val userDir = TempDir.createTempDir

    logger.error("Starting benchmark.")

    lazy val ergoSettings: ErgoSettings = ErgoSettings.read(None).copy(directory =  userDir.getAbsolutePath)

    val timeProvider = new NetworkTimeProvider(ergoSettings.scorexSettings.ntp)

    val peer = ConnectedPeer(
      new InetSocketAddress("localhost", 9001),
      benchRef,
      Incoming,
      Handshake("hey", scorex.core.app.Version.apply("1.0.0"), "assault", None, System.currentTimeMillis())
    )

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

    logger.error("Starting to read modifiers.")
    val modifiers = readModifiers(peer)
    logger.error("Finished read modifiers, starting to bench.")
    logger.error(s"$threshold modifiers to go")
    runBench(benchRef, nodeViewHolderRef, modifiers)
    writeHtml
  }

  private def readModifiers(peer: ConnectedPeer): Vector[ModifiersFromRemote] = {
    var counter = 0
    Source
    .fromInputStream(getClass.getResourceAsStream("/bench/modifiers.txt"))
    .getLines()
    .map { s =>
      counter += 1
      if (counter % 1000 == 0) {
        logger.error(s"ALREADY PROCESS $counter lines")
      }
      val bytes = Base58.decode(s).get
      val typeId = ModifierTypeId @@ bytes.head
      val array = bytes.tail
      ModifiersFromRemote(peer, typeId, Seq(array))
    }.toVector
  }

  private def runBench(benchRef: ActorRef, nodeRef: ActorRef, modifiers: Vector[ModifiersFromRemote]): Unit = {
    benchRef ! Sub(nodeRef)
    benchRef ! Start(nodeRef, modifiers)
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
