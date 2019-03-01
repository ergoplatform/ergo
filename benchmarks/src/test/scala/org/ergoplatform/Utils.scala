package org.ergoplatform

import java.io.InputStream
import java.net.URL

import javax.net.ssl.HttpsURLConnection

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

  def readBytes(stream: InputStream): Array[Byte] = Stream.continually(stream.read().toByte).toArray

}
