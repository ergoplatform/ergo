package org.ergoplatform.utils

import java.io.{BufferedWriter, FileWriter, File, Writer}

import scalan.util.FileUtil

import scala.util.{DynamicVariable, Try}
import scorex.core.validation.ValidationResult
import scorex.util.ModifierId

import scala.collection.mutable

object metrics {

  /** Type-class which implements capability to output data as CSV-like row. */
  trait MetricRow[D] {
    /** Returns an array of columns in a CSV-like table */
    def fieldNames: Seq[String]
    /** Returns a CSV-like row for the given data instance.
      * @param d an instance to transform into a row of strings
      * @return an array of cells in a CSV-like row
      */
    def fieldValues(d: D): Seq[String]
  }

  /** Represents a store for the given metric name and data type.
    * @tparam D
    * @param metricName a name of a metric
    * @param row an instance of CSV-like row handler
    */
  case class MetricStore[D](metricName: String)(implicit row: MetricRow[D]) {
    def fieldNames: Seq[String] = row.fieldNames
    def fieldValues(d: D): Seq[String] = row.fieldValues(d)
  }

  val emptyModifierId: ModifierId = ModifierId @@ ""

  /** Metric data for block operations. */
  case class BlockMetricData(blockId: ModifierId, height: Int, nTransactionsOpt: Option[Int])

  implicit object BlockMetricRow extends MetricRow[BlockMetricData] {
    override val fieldNames: Seq[String] = Array("blockId", "height", "tx_num")
    override def fieldValues(d: BlockMetricData): Seq[String] = {
      val nTx = d.nTransactionsOpt.map(_.toString).getOrElse("")
      Array(d.blockId, d.height.toString, nTx)
    }
  }

  /** Metric data for transaction operations. */
  case class TransactionMetricData(blockId: ModifierId, txId: ModifierId)

  implicit object TransactionMetricRow extends MetricRow[TransactionMetricData] {
    override val fieldNames: Seq[String] = Array("blockId", "txId")
    override def fieldValues(d: TransactionMetricData): Seq[String] = {
      Array(d.blockId, d.txId)
    }
  }

  /** Metric data for an operation with transaction input with the given `index` */
  case class InputMetricData(blockId: ModifierId, txId: ModifierId, index: Int)

  implicit object InputMetricRow extends MetricRow[InputMetricData] {
    override val fieldNames: Seq[String] = Array("blockId", "txId", "index")
    override def fieldValues(d: InputMetricData): Seq[String] =
      Array(d.blockId, d.txId, d.index.toString)
  }

  /** Data obtained by measured operation. Consists of the metric data, the cost computed by the
    * operation and measured time.
    *
    * @tparam D type of metric data
    * @param data     and instance of metric data
    * @param cost     computed cost of operation
    * @param timeNano measured operation time in nanoseconds
    */
  case class MeasuredData[D](data: D, cost: Long, timeNano: Long)

  /** Base class of metric collectors. */
  abstract class MetricsCollector {
    /** Collects the given data, associates it with the given store. */
    def collect[D](data: MeasuredData[D], store: MetricStore[D]): Unit
  }

  object MetricsCollector {
    /** Number of data rows the writer is flushed. */
    val FlushInterval = 50

    private[ergoplatform] val _current = new DynamicVariable[MetricsCollector](null)
    def current: MetricsCollector = {
      val c = _current.value
      if (c == null) DiscardingCollector
      else c
    }
    private object DiscardingCollector extends MetricsCollector {
      override def collect[D](obj: MeasuredData[D], store: MetricStore[D]): Unit = () // do nothing
    }
  }

  /** Base class for CSV file collectors. Each collector maintains a dictionary of stores
    * which may receive data metrics.
    * This class is thread safe. */
  abstract class CsvCollector extends MetricsCollector {

    /** Keeps track of store, associated writer and the current number of lines written. */
    class OutputManager(val store: MetricStore[_], val writer: Writer, var lineCount: Int = 0)

    /** Keeps one OutputManager for each metric name. */
    val metricOutputs = mutable.HashMap.empty[String, OutputManager]

    /** Creates a suitable Writer for the given store. */
    protected def createOutputWriter[D](store: MetricStore[D]): Writer

    /** Creates a suitable OutputManager for the given store. */
    protected def createOutputManager[D](store: MetricStore[D]): OutputManager = {
      new OutputManager(store, createOutputWriter(store))
    }

    /** Write header and thus prepare writer for accepting data rows */
    protected def writeHeader[D](store: MetricStore[D], w: Writer): Unit = {
      val fields = store.fieldNames ++ Array("cost", "time")
      val line = fields.mkString(";") + System.lineSeparator()
      w.write(line)
    }

    override def collect[D](obj: MeasuredData[D], r: MetricStore[D]): Unit = this.synchronized {
      val m = metricOutputs.getOrElseUpdate(r.metricName, createOutputManager(r))

      val values = r.fieldValues(obj.data) ++ Array(obj.cost.toString, obj.timeNano.toString)
      val line = values.mkString(";") + System.lineSeparator()
      m.writer.write(line)
      m.lineCount += 1
      if (m.lineCount % MetricsCollector.FlushInterval == 0)
        m.writer.flush()
    }

    /** Flush all the underlying file writers. */
    def flush(): Unit = synchronized {
      metricOutputs.values.foreach { m =>
        m.writer.flush()
      }
    }

    /** Close all the underlying file writers. */
    def close(): Unit = synchronized {
      metricOutputs.values.foreach { m =>
        m.writer.close()
      }
    }
  }

  /** Concrete implementation of CsvCollector saving all data rows into CSV files in the given
    * directory
    */
  class CsvFileCollector(val directory: String) extends CsvCollector {

    /** Returns a File for the given store. */
    def getMetricFile[D](r: MetricStore[D]): File = {
      val fileName = s"${r.metricName}.csv"
      val dir = FileUtil.file(directory)
      val file = FileUtil.file(dir, fileName)
      file
    }

    /** Creates a new FileWriter wrapped in a BufferedWriter */
    override protected def createOutputWriter[D](r: MetricStore[D]): Writer = {
      val file = getMetricFile(r)
      if(file.exists())
        new BufferedWriter(new FileWriter(file, true))
      else {
        file.getParentFile.mkdirs() // ensure dirs

        // open new empty file
        val w = new BufferedWriter(new FileWriter(file, false))

        writeHeader(r, w)
        w
      }
    }
  }

  /** Executes the given code block under the given collector.
    * Within the block the collector can be obtaned as MetricsCollector.current */
  def executeWithCollector[R](collector: MetricsCollector)(block: => R): R = {
    MetricsCollector._current.withValue(collector)(block)
  }

  /** Execute block and measure the time of its execution using Java Virtual Machine's
    * high-resolution time source, in nanoseconds. */
  def measureTimeNano[T](action: => T): (T, Long) = {
    val t0 = System.nanoTime()
    val res = action
    val t = System.nanoTime()
    (res, t - t0)
  }

  /** Executes the given `block` of code (operation) measuring its time.
    * Sends the given metric data to the given store of the current collector. */
  def measureOp[D, R](d: => D, r: MetricStore[D])(block: => Try[R]): Try[R] = {
    val (res, time) = measureTimeNano(block)
    val obj = MeasuredData(d, -1, time)
    val metricCollector = MetricsCollector.current
    metricCollector.collect(obj, r)
    res
  }

  /** Executes the given `costedOp` measuring its time.
    * Sends the given metric data to the given store of the current collector.
    * In addition, the cost returned by the operation is stored in the row.
    */
  def measureCostedOp[D, R](d: => D, r: MetricStore[D])(costedOp: => Try[Long]): Try[Unit] = {
    val (costTry, time) = measureTimeNano(costedOp)
    costTry.map { cost =>
      val obj = MeasuredData(d, cost, time)
      val metricCollector = MetricsCollector.current
      metricCollector.collect(obj, r)
      ()
    }
  }

  /** Executes the given `validationOp` measuring its time.
    * Sends the given metric data to the given store of the current collector.
    * In addition, the cost returned by the operation is stored in the row.
    */
  def measureValidationOp[D, R](d: => D, r: MetricStore[D])
                               (validationOp: => ValidationResult[Long]): ValidationResult[Long] = {
    val (res, time) = measureTimeNano(validationOp)
    res.map { cost =>
      val obj = MeasuredData(d, cost, time)
      val metricCollector = MetricsCollector.current
      metricCollector.collect(obj, r)
      cost
    }
  }
}
