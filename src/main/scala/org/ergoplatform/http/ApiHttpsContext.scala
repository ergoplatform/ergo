package org.ergoplatform.http

import akka.http.scaladsl.{ConnectionContext, HttpsConnectionContext}
import org.ergoplatform.settings.ApiHttpsSettings
import scorex.util.ScorexLogging

import java.io.FileInputStream
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{KeyManagerFactory, SSLContext}
import scala.util.control.NonFatal

/**
  * Builds a server-side TLS context for the REST API from a Java keystore
  * (PKCS12 or JKS) holding the server certificate and private key.
  */
object ApiHttpsContext extends ScorexLogging {

  private val DefaultKeyStoreType = "PKCS12"

  /**
    * Build the HTTPS context from settings, failing fast with a clear,
    * operator-facing message if the keystore is misconfigured or unreadable.
    * Intended to be called during startup, before the rest of the node is
    * bootstrapped, so a bad configuration aborts cleanly.
    */
  def fromSettings(settings: ApiHttpsSettings): HttpsConnectionContext = {
    val keyStorePath = settings.keyStorePath.getOrElse(
      configError(
        "scorex.restApi.https.keyStorePath must be set when https.enabled = true"
      )
    )
    val password = settings.keyStorePassword
      .getOrElse(
        configError(
          "scorex.restApi.https.keyStorePassword must be set when https.enabled = true"
        )
      )
      .toCharArray
    val keyStoreType = settings.keyStoreType.getOrElse(DefaultKeyStoreType)

    try {
      val keyStore       = KeyStore.getInstance(keyStoreType)
      val keyStoreStream = new FileInputStream(keyStorePath)
      try {
        keyStore.load(keyStoreStream, password)
      } finally {
        keyStoreStream.close()
      }

      val keyManagerFactory = KeyManagerFactory.getInstance(
        KeyManagerFactory.getDefaultAlgorithm
      )
      keyManagerFactory.init(keyStore, password)

      val sslContext = SSLContext.getInstance("TLS")
      sslContext.init(keyManagerFactory.getKeyManagers, null, new SecureRandom)

      log.info(s"Loaded REST API TLS keystore from $keyStorePath")
      ConnectionContext.httpsServer(sslContext)
    } catch {
      case NonFatal(e) =>
        configError(
          s"Failed to initialize REST API HTTPS from keystore '$keyStorePath' " +
          s"(type $keyStoreType): ${e.getMessage}"
        )
    }
  }

  private def configError(msg: String): Nothing =
    throw new IllegalArgumentException(s"Malformed REST API HTTPS configuration: $msg")
}
