package scorex.core.api.http

import akka.http.scaladsl.server.{AuthorizationFailedRejection, Directive0}
import org.ergoplatform.settings.RESTApiSettings
import org.ergoplatform.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256

trait ApiDirectives extends CorsHandler with ScorexEncoding {
  val settings: RESTApiSettings
  val apiKeyHeaderName: String

  lazy val withAuth: Directive0 = optionalHeaderValueByName(apiKeyHeaderName).flatMap {
    case _ if settings.apiKeyHash.isEmpty || settings.apiKeyHash.contains(ApiDirectives.LegacyDefaultKeyHash) =>
      reject(AuthorizationFailedRejection)
    case None => reject(AuthorizationFailedRejection)
    case Some(key) =>
      val keyHashStr: String = encoder.encode(Blake2b256(key))
      if (settings.apiKeyHash.contains(keyHashStr)) {
        pass
      } else {
        reject(AuthorizationFailedRejection)
      }
  }

}

object ApiDirectives {
  // The former bundled credential is public and must not grant privileged access.
  private val LegacyDefaultKeyHash = "324dcf027dd4a30a932c441f365a25e86b173defa4b8e58948253471b81b72cf"
}
