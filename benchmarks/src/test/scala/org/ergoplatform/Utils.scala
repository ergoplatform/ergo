package org.ergoplatform

import java.io.InputStream
import java.net.URL

import com.google.common.primitives.Ints
import javax.net.ssl.HttpsURLConnection
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer

object Utils {

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

}
