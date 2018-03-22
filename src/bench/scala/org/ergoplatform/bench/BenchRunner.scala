package org.ergoplatform.bench

import java.io.File
import java.net.URL

import akka.actor.{ActorRef, ActorSystem}
import javax.net.ssl.HttpsURLConnection
import org.ergoplatform.bench.misc.ModifierWriter
import org.ergoplatform.bench.protocol.{Start, SubTo}
import org.ergoplatform.mining.EquihashPowScheme
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.settings.{ChainSettings, ErgoSettings}
import scorex.core.LocallyGeneratedModifiersMessages.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.EventType
import scorex.core.utils.{NetworkTimeProvider, NetworkTimeProviderSettings, ScorexLogging}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

object BenchRunner extends ScorexLogging {

  implicit val system: ActorSystem = ActorSystem("bench")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val targetDirectory = "target/bench"

  def main(args: Array[String]): Unit = {

    new File(targetDirectory).mkdirs()

    val threshold = args.headOption.getOrElse("1000").toInt
    val fileName = args.lift(1).get

    val benchRef = BenchActor(threshold)
    val userDir = TempDir.createTempDir

    log.info(s"User dir is $userDir")
    log.info("Starting benchmark.")

    lazy val ergoSettings: ErgoSettings = ErgoSettings.read(None).copy(
      directory =  userDir.getAbsolutePath,
      chainSettings = ChainSettings(1 minute, 1, 100, new EquihashPowScheme(96.toChar, 5.toChar))
    )

    log.info(s"Setting that being used:")
    log.info(s"$ergoSettings")


    val ntpSettings = NetworkTimeProviderSettings("pool.ntp.org", 30 minutes, 30 seconds)
    val timeProvider = new NetworkTimeProvider(ntpSettings)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

    log.info("Starting to read modifiers.")
    val modifiers = readModifiers(fileName, threshold)
    log.info("Finished read modifiers, starting to bench.")
    log.info(s"$threshold modifiers to go")
    runBench(benchRef, nodeViewHolderRef, modifiers)
    //writeHtml
  }

  private def readModifiers(fileName: String, threshold: Int): Vector[ErgoPersistentModifier] = {
    var counter = 0
    var headers = 0
    log.info("Start reading modifiers from data file.")
    val is = getUrlInputStream(fileName)
    val result = Stream
      .continually{
        counter += 1
        if (counter % 100 == 0) { log.error(s"Already read $counter blocks.")}
        val mod = ModifierWriter.read(is)
        if (mod.map{ m => m.modifierTypeId == Header.modifierTypeId}.getOrElse(false)) { headers += 1}
        mod
      }
      .takeWhile(m => (headers <= threshold) && m.isDefined)
      .flatten
      .toVector

    log.error(s"Total modificators ${result.length}")

    result
  }

  private def runBench(benchRef: ActorRef, nodeRef: ActorRef, modifiers: Vector[ErgoPersistentModifier]): Unit = {
    benchRef ! SubTo(nodeRef, Seq(EventType.SuccessfulSemanticallyValidModifier))
    benchRef ! Start
    modifiers.foreach { m => nodeRef ! LocallyGeneratedModifier(m) }
  }

  private def getUrlInputStream(url: String,
                                connectTimeout: Int = 5000,
                                readTimeout: Int = 5000,
                                requestMethod: String = "GET") = {
    val u = new URL(url)
    val conn = u.openConnection.asInstanceOf[HttpsURLConnection]
    conn.setConnectTimeout(connectTimeout)
    conn.setReadTimeout(readTimeout)
    conn.setRequestMethod(requestMethod)
    conn.connect
    conn.getInputStream
  }
}
