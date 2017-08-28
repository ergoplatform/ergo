package org.ergoplatform.nodeView.history

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos
import scorex.core.utils.ScorexLogging

import scala.util.Try

/**
  * Storage for node indexes and difficulties
  */
class IndexStorage(storage: LSMStore) extends ScorexLogging {
}
