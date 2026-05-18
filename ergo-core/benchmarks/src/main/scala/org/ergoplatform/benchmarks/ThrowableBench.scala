package org.ergoplatform.benchmarks

import java.util.concurrent.TimeUnit

import org.ergoplatform.modifiers.HeaderTypeId
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.validation.MalformedModifierError
import org.ergoplatform.validation.RecoverableModifierError
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scorex.util.ModifierId

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ThrowableBench {

  private val msg: String = "validation failure"
  private val mid: ModifierId =
    ModifierId @@ "0000000000000000000000000000000000000000000000000000000000000000"
  private val tid: NetworkObjectTypeId.Value = HeaderTypeId.value

  private val preallocatedRecoverable: RecoverableModifierError =
    new RecoverableModifierError(msg, mid, tid, None)

  @Benchmark
  def throwMalformed(bh: Blackhole): Unit = {
    try {
      throw new MalformedModifierError(msg, mid, tid, None)
    } catch {
      case t: Throwable => bh.consume(t)
    }
  }

  @Benchmark
  def throwRecoverable(bh: Blackhole): Unit = {
    try {
      throw new RecoverableModifierError(msg, mid, tid, None)
    } catch {
      case t: Throwable => bh.consume(t)
    }
  }

  @Benchmark
  def throwSingleton(bh: Blackhole): Unit = {
    try {
      throw preallocatedRecoverable
    } catch {
      case t: Throwable => bh.consume(t)
    }
  }
}
