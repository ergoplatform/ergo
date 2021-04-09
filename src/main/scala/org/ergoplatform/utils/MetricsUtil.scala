package org.ergoplatform.utils

import java.io.{BufferedWriter, FileWriter, Writer}

import scalan.util.{BenchmarkUtil, FileUtil}

import scala.util.{DynamicVariable, Try}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.settings.ErgoSettings

import scala.collection.mutable

object metrics {

  trait Reporter[D] {
    def metricName: String
    def fieldNames: Seq[String]
    def fieldValues(d: D): Seq[String]
  }

  case class ErgoFullBlockReporter(metricName: String) extends Reporter[ErgoFullBlock] {
    override def fieldNames: Seq[String] = ErgoFullBlockReporter.names
    override def fieldValues(b: ErgoFullBlock): Seq[String] = Array(b.id)
  }
  object ErgoFullBlockReporter {
    private val names = Array("id")
  }

  object ApplyTransactionsReporter extends Reporter[ErgoFullBlock] {
    override def metricName: String = "applyTransactions"
    override val fieldNames: Seq[String] = Array("blockId", "tx_num")
    override def fieldValues(b: ErgoFullBlock): Seq[String] =
      Array(b.id, b.blockTransactions.txs.length.toString)
  }

  case class MeasuredObject[D](data: D, cost: Long, time: Long)

  abstract class MetricsCollector {
    def save[D: Reporter](obj: MeasuredObject[D]): Unit
  }

  object MetricsCollector {
    private[ergoplatform] val _current = new DynamicVariable[MetricsCollector](null)
    def current: MetricsCollector = {
      val c = _current.value
      if (c == null) Throwing
      else c
    }
    private object Throwing extends MetricsCollector {
      override def save[D: Reporter](obj: MeasuredObject[D]): Unit = () // do nothing
    }
  }

  abstract class CsvCollector(val ergoSettings: ErgoSettings) extends MetricsCollector {

    protected def createOutputWriter[D](r: Reporter[D]): Writer

    protected def createOutputManager[D](r: Reporter[D]): OutputManager = {
      new OutputManager(createOutputWriter(r))
    }

    /** Write header and thus prepare writer for accepting data rows */
    protected def writeHeader[D](r: Reporter[D], w: Writer) = {
      val fields = r.fieldNames ++ Array("cost", "time")
      val line = fields.mkString(";") + System.lineSeparator()
      w.write(line)
    }


    class OutputManager(val writer: Writer, var lineCount: Int = 0)

    val metricOutputs = mutable.HashMap.empty[String, OutputManager]

    override def save[D](obj: MeasuredObject[D])(implicit r: Reporter[D]): Unit = {
      val m = metricOutputs.getOrElseUpdate(r.metricName, createOutputManager(r))

      val values = r.fieldValues(obj.data) ++ Array(obj.cost.toString, obj.time.toString)
      val line = values.mkString(";") + System.lineSeparator()
      m.writer.write(line)
      m.lineCount += 1
      if (m.lineCount % 50 == 0)
        m.writer.flush()
    }

    /** Flush all the underlying file writers. */
    def flush() = {
      metricOutputs.values.foreach { m =>
        m.writer.flush()
      }
    }

    /** Close all the underlying file writers. */
    def close() = {
      metricOutputs.values.foreach { m =>
        m.writer.close()
      }
    }
  }

  class CsvFileCollector(override val ergoSettings: ErgoSettings)
      extends CsvCollector(ergoSettings) {

    override protected def createOutputWriter[D](r: Reporter[D]): Writer = {
      val fileName = s"${r.metricName}.csv"
      val dir = FileUtil.file(ergoSettings.directory)
      val file = FileUtil.file(dir, fileName)

      if(file.exists())
        new BufferedWriter(new FileWriter(file, true))
      else {
        dir.mkdirs() // ensure dirs

        // open new empty file
        val w = new BufferedWriter(new FileWriter(file, false))

        writeHeader(r, w)
        w
      }
    }
  }

  lazy val csvCollector = new CsvFileCollector(ErgoSettings.read())

  def executeWithCollector[R](collector: MetricsCollector)(block: => R): R = {
    MetricsCollector._current.withValue(collector)(block)
  }

  def measureOp[D, R](d: D, r: Reporter[D])(block: => Try[R]): Try[R] = {
    val (res, time) = BenchmarkUtil.measureTimeNano(block)
    val obj = MeasuredObject(d, -1, time)
    val metricCollector = MetricsCollector.current
    metricCollector.save(obj)(r)
    res
  }

  def measureCostedOp[D, R](d: D, r: Reporter[D])(costedOp: => Try[Long]): Try[Unit] = {
    val (costTry, time) = BenchmarkUtil.measureTimeNano(costedOp)
    costTry.map { cost =>
      val obj = MeasuredObject(d, cost, time)
      val metricCollector = MetricsCollector.current
      metricCollector.save(obj)(r)
      ()
    }
  }
}
