package org.ergoplatform

import java.io.{File, InputStream, PrintWriter}
import java.net.URL

import com.google.common.primitives.Ints
import javax.net.ssl.HttpsURLConnection
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer

object Utils {

  final case class BenchReport(benchCase: String, et: Long)

  def getUrlInputStream(url: String,
                        connectTimeout: Int = 5000,
                        readTimeout: Int = 5000,
                        requestMethod: String = "GET"): InputStream = {
    val u = new URL(url)
    val conn = u.openConnection.asInstanceOf[HttpsURLConnection]
    conn.setConnectTimeout(connectTimeout)
    conn.setReadTimeout(readTimeout)
    conn.setRequestMethod(requestMethod)
    conn.connect()
    conn.getInputStream
  }

  def readLength(implicit fis: InputStream): Option[Int] =
    Some(Stream.continually(fis.read().toByte).take(4).toArray).map(Ints.fromByteArray)

  def readBytes(length: Int)(implicit fis: InputStream): Option[Array[Byte]] =
    Some(Stream.continually(fis.read().toByte).take(length).toArray)

  def readModifier[M <: ErgoPersistentModifier](implicit fis: InputStream): Option[M] = {
    for {
      length <- readLength
      bytes <- readBytes(length)
      mod <- HistoryModifierSerializer.parseBytesTry(bytes).toOption.map(_.asInstanceOf[M])
    } yield mod
  }

  def dumpToFile(benchName: String, startTs: Long, reports: Seq[BenchReport]): Unit = {
    new File(s"target/bench/").mkdirs()
    val outWriter = new PrintWriter(new File(s"target/bench/bench-report.json"))
    val reportsStr = reports.map { case BenchReport(benchCase, et) =>
      s"""{"benchCase":"$benchCase","elapsedTime":$et}"""
    }.mkString(",")
    outWriter.write(s"""{"benchName":"$benchName","startTs":$startTs,"reports":[$reportsStr]}""")
    outWriter.close()
  }

  def time[R](block: => R): Double = {
    val t0 = System.nanoTime()
    block // call-by-name
    val t1 = System.nanoTime()
    (t1.toDouble - t0) / 1000000
  }

}
