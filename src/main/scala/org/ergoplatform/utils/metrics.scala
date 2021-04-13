package org.ergoplatform.utils

import java.io.{BufferedWriter, FileWriter, File, Writer}

import scalan.util.{BenchmarkUtil, FileUtil}

import scala.util.{DynamicVariable, Try}
import org.ergoplatform.modifiers.ErgoFullBlock
import scorex.core.validation.ValidationResult
import scorex.util.{ModifierId, bytesToId}

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

  val emptyModifierId: ModifierId = bytesToId(Array.fill(32)(0.toByte))

  case class InputMetricData(blockId: ModifierId, txId: ModifierId, index: Int)

  case class InputMetricReporter(metricName: String) extends Reporter[InputMetricData] {
    override val fieldNames: Seq[String] = Array("blockId", "txId", "index")
    override def fieldValues(d: InputMetricData): Seq[String] =
      Array(d.blockId, d.txId, d.index.toString)
  }

  case class TransactionMetricData(blockId: ModifierId, txId: ModifierId)

  case class TransactionMetricReporter(val metricName: String) extends Reporter[TransactionMetricData] {
    override val fieldNames: Seq[String] = Array("blockId", "txId")
    override def fieldValues(d: TransactionMetricData): Seq[String] = {
      Array(d.blockId, d.txId)
    }
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

  abstract class CsvCollector extends MetricsCollector {

    protected def createOutputWriter[D](r: Reporter[D]): Writer

    protected def createOutputManager[D](r: Reporter[D]): OutputManager = {
      new OutputManager(r, createOutputWriter(r))
    }

    /** Write header and thus prepare writer for accepting data rows */
    protected def writeHeader[D](r: Reporter[D], w: Writer) = {
      val fields = r.fieldNames ++ Array("cost", "time")
      val line = fields.mkString(";") + System.lineSeparator()
      w.write(line)
    }


    class OutputManager(val reporter: Reporter[_], val writer: Writer, var lineCount: Int = 0)

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

  class CsvFileCollector(val directory: String) extends CsvCollector {

    def getMetricFile[D](r: Reporter[D]): File = {
      val fileName = s"${r.metricName}.csv"
      val dir = FileUtil.file(directory)
      val file = FileUtil.file(dir, fileName)
      file
    }

    override protected def createOutputWriter[D](r: Reporter[D]): Writer = {
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

  def measureOp[D, R](d: => D, r: Reporter[D])(block: => Try[R]): Try[R] = {
    val (res, time) = measureTimeNano(block)
    val obj = MeasuredObject(d, -1, time)
    val metricCollector = MetricsCollector.current
    metricCollector.save(obj)(r)
    res
  }

  def measureCostedOp[D, R](d: => D, r: Reporter[D])(costedOp: => Try[Long]): Try[Unit] = {
    val (costTry, time) = measureTimeNano(costedOp)
    costTry.map { cost =>
      val obj = MeasuredObject(d, cost, time)
      val metricCollector = MetricsCollector.current
      metricCollector.save(obj)(r)
      ()
    }
  }

  def measureValidationOp[D, R](d: => D, r: Reporter[D])
                               (costedOp: => ValidationResult[Long]): ValidationResult[Long] = {
    val (res, time) = measureTimeNano(costedOp)
    res.map { cost =>
      val obj = MeasuredObject(d, cost, time)
      val metricCollector = MetricsCollector.current
      metricCollector.save(obj)(r)
      cost
    }
  }
}
