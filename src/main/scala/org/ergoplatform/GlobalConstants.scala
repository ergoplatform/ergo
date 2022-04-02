package org.ergoplatform

/**
  * A singleton which holds constants needed around the whole Ergo Platform.
  */
object GlobalConstants {

  /**
    * Name of dispatcher for actors processing API requests
    * (to avoid clashing between blockchain processing and API actors)
    */
  val ApiDispatcher = "api-dispatcher"
}
