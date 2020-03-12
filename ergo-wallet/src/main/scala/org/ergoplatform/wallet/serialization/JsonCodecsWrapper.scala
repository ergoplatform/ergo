package org.ergoplatform.wallet.serialization

import org.ergoplatform.JsonCodecs

/**
  * JSON Codecs provided as singleton package, not trait.
  * Could be useful for Java applications willing to use json codecs for ergo-related objects.
  */
object JsonCodecsWrapper extends JsonCodecs
