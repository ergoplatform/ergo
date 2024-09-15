package org.ergoplatform.utils

import scorex.util.encode.BytesEncoder

/**
  * Trait with bytes to string encoder
  * TODO extract to ScorexUtils project
  */
trait ScorexEncoding {
  val encoder: BytesEncoder = ScorexEncoder
}
