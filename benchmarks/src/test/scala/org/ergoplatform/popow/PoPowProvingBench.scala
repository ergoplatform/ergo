package org.ergoplatform.popow

import org.ergoplatform.modifiers.history.{PoPowAlgos, PoPowHeader}
import org.ergoplatform.nodeView.NVBenchmark
import org.ergoplatform.settings.PoPowParams
import org.scalameter.KeyValue
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object PoPowProvingBench extends Bench.ForkedTime with NVBenchmark {

  private val config = Seq[KeyValue](
    exec.minWarmupRuns -> 10,
    exec.maxWarmupRuns -> 30,
    exec.benchRuns -> 20,
    exec.requireGC -> true
  )

  private val chainLength = Gen.enumeration("chain length")(200, 500, 1000)

  private val poPowParams = PoPowParams(30, 30, 30, .45)

  private val chain = readBlocks.map { b =>
    PoPowHeader(b.header, PoPowAlgos.unpackInterlinks(b.extension.fields).get)
  }

  measure method "PoPowAlgos.prove" in {
    using(chainLength) config (config: _*) in { len =>
      PoPowAlgos.prove(chain.take(len))(poPowParams)
    }
  }

}
