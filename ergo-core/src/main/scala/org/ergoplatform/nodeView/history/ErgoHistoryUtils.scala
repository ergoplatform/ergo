package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryConstants.EmptyHistoryHeight

object ErgoHistoryUtils {
  def heightOf(headerOpt: Option[Header]): Int = headerOpt.map(_.height).getOrElse(EmptyHistoryHeight)
}
