import org.scalameter
import org.scalameter.Key._
import org.scalameter.api.Bench
import org.scalameter.execution.SeparateJvmsExecutor
import org.scalameter.picklers.Implicits._
import org.scalameter.{Aggregator, Executor, Gen, Persistor}
import scorex.crypto.authds.benchmarks.Helper._

class MemoryFootprint extends Bench[Double] {

  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.median[Double],
    measurer)
  lazy val measurer = new scalameter.Measurer.MemoryFootprint
  lazy val reporter = new CsvReporter()
  lazy val persistor = Persistor.None

  val sizes = Gen.enumeration("size")(100000, 500000, 1000000)

  performance of "MemoryFootprint" in {
    performance of "AVLTree" in {
      using(sizes) config(
        exec.benchRuns -> 4,
        exec.independentSamples -> 2
      ) in { prover }
    }
  }
}
