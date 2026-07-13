package scorex.core.api.http

import akka.http.scaladsl.server.{AuthorizationFailedRejection, Directive0}
import org.ergoplatform.settings.RESTApiSettings
import org.ergoplatform.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.core.api.http.ApiDirectives.DisabledApiKeyHashes

trait ApiDirectives extends CorsHandler with ScorexEncoding {
  val settings: RESTApiSettings
  val apiKeyHeaderName: String

  lazy val withAuth: Directive0 = optionalHeaderValueByName(apiKeyHeaderName).flatMap {
    case _ if settings.apiKeyHash.exists(DisabledApiKeyHashes.contains) =>
      reject(AuthorizationFailedRejection)
    case _ if settings.apiKeyHash.isEmpty => pass
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
  /** Explicitly disables routes guarded by [[ApiDirectives.withAuth]]. */
  val DisabledApiKeyHash: String = "0" * 64

  /** Hash of the public API key shipped by older node versions. */
  val LegacyDefaultApiKeyHash: String =
    "324dcf027dd4a30a932c441f365a25e86b173defa4b8e58948253471b81b72cf"

  val DisabledApiKeyHashes: Set[String] = Set(DisabledApiKeyHash, LegacyDefaultApiKeyHash)
}
