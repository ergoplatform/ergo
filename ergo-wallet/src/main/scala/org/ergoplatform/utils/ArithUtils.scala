package org.ergoplatform.utils

object ArithUtils {

  /**
    * Add longs, returning Long.Max value if there was any long overflow
    */
  @inline def addExact(a: Long, b: Long): Long = {
    val sum = a + b
    if (((a ^ sum) & (b ^ sum)) < 0) Long.MaxValue else sum
  }

  def addExact(a: Long, b: Long, c: Long*): Long = c.foldLeft(addExact(a,b))((x, y) => addExact(x, y))

  /**
    * Multiply longs, returning Long.Max value if there was any long overflow
    */
  @inline def multiplyExact(e1: Long, e2: Long): Long = {
    try {
      Math.multiplyExact(e1, e2)
    } catch {
      case _: Throwable => Long.MaxValue
    }
  }

}
