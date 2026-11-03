package org.ergoplatform.http.routes

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Directives, Route}
import org.ergoplatform.settings.ErgoSettings
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16

object ApiTestAuth {
  val ApiKeyHeaderName: String = "api_key"
  val ApiKey: String = "route-spec-api-key"
  val ApiKeyHash: String = Base16.encode(Blake2b256(ApiKey))

  def withApiKey(route: Route): Route =
    Directives.mapRequest(_.addHeader(RawHeader(ApiKeyHeaderName, ApiKey))) {
      route
    }

  def settingsWithApiKey(settings: ErgoSettings): ErgoSettings =
    settings.copy(
      scorexSettings = settings.scorexSettings.copy(
        restApi = settings.scorexSettings.restApi.copy(apiKeyHash = Some(ApiKeyHash))
      )
    )
}
