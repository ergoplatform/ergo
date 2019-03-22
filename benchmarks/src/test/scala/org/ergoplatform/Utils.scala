package org.ergoplatform

import java.io.{File, InputStream, PrintWriter}
import java.net.URL

import com.google.common.primitives.Ints
import javax.net.ssl.HttpsURLConnection
import org.asynchttpclient.{AsyncHttpClient, DefaultAsyncHttpClient}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import scala.util.Properties

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
    val outWriter = new PrintWriter(new File(s"target/bench/bench-report.json"))
    val reportsStr = reports.map { case BenchReport(benchCase, et) =>
      s"""{"benchCase":"$benchCase","elapsedTime":$et}"""
    }.mkString(",")
    outWriter.write(s"""{"benchName":"$benchName","startTs":$startTs,"reports":[$reportsStr]}""")
    outWriter.close()

    sendToInfluxDb(benchName, startTs, reports)
  }

  def sendToInfluxDb(benchName: String, startTs: Long, reports: Seq[BenchReport]): Unit = {
    val influxdbConnectionString = Properties.envOrElse("INFLUXDB_CONNECTION_STRING", "")
    val client: AsyncHttpClient = new DefaultAsyncHttpClient
    val whenResponse = client.prepareGet("http://ergoplatform.com/").execute()
    // _post(s"$url:$port$path").setHeader("api_key", apiKey)

    val outWriter = new PrintWriter(new File(s"target/bench/bench-report-influxdb.json"))

    val reportsStr = reports.map { case BenchReport(benchCase, et) =>
      s"""{"benchCase":"$benchCase","elapsedTime":$et}"""
    }.mkString(",")
    outWriter.write(s""" $influxdbConnectionString cpu_load_short,host=server01,region=us-west value=0.64 1434055562000000000    {"benchName":"$benchName","startTs":$startTs,"reports":[$reportsStr]}""")

    outWriter.close()
  }

}
