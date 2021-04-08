package org.ergoplatform.utils

import scalan.util.BenchmarkUtil

import scala.util.{DynamicVariable, Try}
import org.ergoplatform.modifiers.ErgoFullBlock

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

  object CsvCollector extends MetricsCollector {
    override def save[D: Reporter](obj: MeasuredObject[D]): Unit = () // do nothing
  }

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
