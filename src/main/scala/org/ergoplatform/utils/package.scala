package org.ergoplatform

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.authds._

package object utils {
  implicit def byteArrayWrapper(a: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(a)

  implicit def idToTag(id: ModifierId): VersionTag = VersionTag @@ id

  implicit def digestToTag(id: ADDigest): VersionTag = VersionTag @@ id

}
