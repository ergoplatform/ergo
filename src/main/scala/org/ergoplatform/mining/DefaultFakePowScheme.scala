package org.ergoplatform.mining

import org.ergoplatform.modifiers.history.Header

object DefaultFakePowScheme extends AutoleakusPowScheme(1, 1) {
  override def verify(header: Header): Boolean = true
}