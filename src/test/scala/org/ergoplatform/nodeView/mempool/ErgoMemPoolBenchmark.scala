package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.ErgoTransactionGenerators
import org.scalameter.KeyValue
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import scorex.core.ModifierId
import org.ergoplatform.utils.ErgoTestHelpers.defaultExecutionContext

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Random => Rng}

object ErgoMemPoolBenchmark
  extends Bench.ForkedTime
    with ErgoTransactionGenerators {

  private val blockSizes = Gen.enumeration("txs in block")(50, 100, 200)
  private val waitingSizes = Gen.enumeration("waitings")(1, 10)

  private def waitForTransactionsInSequence(txIncomeOrder: Seq[Seq[ErgoTransaction]] => Seq[ErgoTransaction]) = for {
    waitingSize <- waitingSizes
    transactionsPerBlock <- blockSizes
  } yield {
    val txsByWaitingGroups = for {_ <- 0 until waitingSize}
      yield {
        (for {_ <- 0 until transactionsPerBlock} yield {
          invalidErgoTransactionGen.sample
        }).flatten
      }
    (txsByWaitingGroups.map(_.map(_.id)), txIncomeOrder(txsByWaitingGroups))
  }

  private val bestCaseGenerator = waitForTransactionsInSequence(_.flatten)
  private val avgCaseGenerator = waitForTransactionsInSequence(txs => Rng.shuffle(txs.flatten))
  private val worstCaseGenerator = waitForTransactionsInSequence(groups => {
    groups.flatMap(_.init) ++ groups.map(_.last)
  })

  private val config = Seq[KeyValue](
    exec.minWarmupRuns -> 10,
    exec.maxWarmupRuns -> 30,
    exec.benchRuns -> 20,
    exec.requireGC -> true
  )

  private def bench(txsByWaitingGroups: Seq[Seq[ModifierId]],
                    txsInIncomeOrder: Seq[ErgoTransaction]) = {
    val pool = ErgoMemPool.empty
    val futures = txsByWaitingGroups.map(group => {
      pool.waitForAll(group)
    })
    val resultFuture = Future.sequence(futures)
    txsInIncomeOrder.foreach(pool.put)
    Await.result(resultFuture, Duration.Inf)
  }

  performance of "ErgoMemPool awaiting" in {
    performance of "best case" in {
      using(bestCaseGenerator) config (config: _*) in {
        case (txsByWaitingGroups, txsInIncomeOrder) => bench(txsByWaitingGroups, txsInIncomeOrder)
      }
    }

    performance of "avg case" in {
      using(avgCaseGenerator) config (config: _*) in {
        case (txsByWaitingGroups, txsInIncomeOrder) => bench(txsByWaitingGroups, txsInIncomeOrder)
      }
    }

    performance of "worst case" in {
      using(worstCaseGenerator) config (config: _*) in {
        case (txsByWaitingGroups, txsInIncomeOrder) => bench(txsByWaitingGroups, txsInIncomeOrder)
      }
    }
  }
}
