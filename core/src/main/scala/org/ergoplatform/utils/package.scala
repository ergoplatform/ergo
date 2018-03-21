package org.ergoplatform

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.{ModifierId, VersionTag}

package object utils {
  implicit def byteArrayWrapper(a: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(a)

  implicit def idToTag(id: ModifierId): VersionTag = VersionTag @@ id

}
