import java.io.PrintWriter

import org.scalameter.{CurveData, Persistor, log}
import org.scalameter.api.Reporter
import org.scalameter.utils.Tree

class CsvReporter extends Reporter[Double] {

  override def report(result: CurveData[Double], persistor: Persistor): Unit = {
    for (measurement <- result.measurements) {
      log(s"${measurement.params}: ${measurement.value} ${measurement.units}")
    }

    val writer = new PrintWriter("target/memory_avltree_results.csv")
    val headerLine = result.measurements.map(_.params.axisData.head._2.toString).mkString(",")
    val valuesLine = result.measurements.map(_.value).mkString(",")
    writer.println(headerLine)
    writer.println(valuesLine)
    writer.flush()
    writer.close()
  }

  override def report(results: Tree[CurveData[Double]], persistor: Persistor): Boolean = true

}
