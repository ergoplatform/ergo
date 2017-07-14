package org.ergoplatform

import io.iohk.iodb.ByteArrayWrapper

package object utils {
  implicit def byteArrayWrapper(a: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(a)

}
