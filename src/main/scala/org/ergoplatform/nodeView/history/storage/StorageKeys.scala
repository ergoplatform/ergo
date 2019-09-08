package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Constants.HashLength

object StorageKeys {

  val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(Header.modifierTypeId))

  val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(-1))
}
