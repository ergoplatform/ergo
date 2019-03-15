package org.ergoplatform.nodeView

import org.ergoplatform.Utils
import org.ergoplatform.modifiers.ErgoPersistentModifier

trait NVBenchmark {

  def time[R](block: => R): Double = {
    val t0 = System.nanoTime()
    block // call-by-name
    val t1 = System.nanoTime()
    (t1.toDouble - t0) / 1000000
  }

  def readModifiers[M <: ErgoPersistentModifier](path: String): Seq[M] = {
    val is = Utils.getUrlInputStream(path)
    Stream
      .continually {
        Utils.readModifier[M](is)
      }
      .takeWhile(_.isDefined)
      .flatten
      .toList
  }

}
