package org.ergoplatform.utils

import org.scalacheck.Shrink

trait NoShrink {
  protected implicit def noShrink[A]: Shrink[A] = NoShrink[A]

}

object NoShrink {
  def apply[A]: Shrink[A] = Shrink(_ => Stream.empty)
}
