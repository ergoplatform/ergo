package org.ergoplatform.nodeView

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.Files

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class BlockchainResetSpec extends AnyFlatSpec with Matchers {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val failure = new IllegalStateException("injected rollback failure")

  it should "leave a durable restart guard after a pruning failure" in {
    val dir = Files.createTempDirectory("reset-history-failure-")
    try {
      BlockchainReset.pruneHistory[String](dir.toString, 2)(Failure(failure)) shouldBe Failure(failure)
      intercept[IllegalArgumentException](BlockchainReset.requireCompleteHistory(dir.toString))
      var retried = false
      BlockchainReset.pruneHistory(dir.toString, 2) { retried = true; Success(()) }.isFailure shouldBe true
      retried shouldBe false
    } finally {
      Files.deleteIfExists(dir.resolve("history-reset.incomplete"))
      Files.delete(dir)
    }
  }

  it should "remove the restart guard only after successful pruning" in {
    val dir = Files.createTempDirectory("reset-history-success-")
    try {
      BlockchainReset.pruneHistory(dir.toString, 2) {
        Files.exists(dir.resolve("history-reset.incomplete")) shouldBe true
        Success("history")
      } shouldBe Success("history")
      BlockchainReset.requireCompleteHistory(dir.toString)
    } finally {
      Files.deleteIfExists(dir.resolve("history-reset.incomplete"))
      Files.delete(dir)
    }
  }

  it should "reject invalid or unavailable targets before mutation" in {
    BlockchainReset.validateTarget(0, 10, hasFullBlock = false, canRollback = true).isFailure shouldBe true
    BlockchainReset.validateTarget(-1, 10, hasFullBlock = false, canRollback = true).isFailure shouldBe true
    BlockchainReset.validateTarget(11, 10, hasFullBlock = true, canRollback = true).isFailure shouldBe true
    BlockchainReset.validateTarget(5, 10, hasFullBlock = false, canRollback = true).isFailure shouldBe true
    BlockchainReset.validateTarget(5, 10, hasFullBlock = true, canRollback = false).isFailure shouldBe true
    BlockchainReset.validateTarget(5, 10, hasFullBlock = true, canRollback = true) shouldBe Success(())
  }

  it should "retain history until the wallet has acknowledged its persisted rollback" in {
    val wallet = Promise[Try[Unit]]()
    val walletStarted = Promise[Unit]()
    val pruned = Promise[Unit]()
    val result = BlockchainReset.run(
      () => Success("state"),
      () => { walletStarted.success(()); wallet.future },
      () => { pruned.success(()); Success("history") }
    )
    Await.result(walletStarted.future, 5.seconds)
    result.isCompleted shouldBe false
    pruned.isCompleted shouldBe false
    wallet.success(Success(()))
    Await.result(result, 5.seconds) shouldBe Success(("state", "history"))
    pruned.isCompleted shouldBe true
  }

  it should "stop at a state failure without contacting the wallet or deleting history" in {
    val calls = ArrayBuffer.empty[String]
    val result = BlockchainReset.run[String, String](
      () => { calls += "state"; Failure(failure) },
      () => { calls += "wallet"; Future.successful(Success(())) },
      () => { calls += "history"; Success("history") }
    )
    Await.result(result, 5.seconds) shouldBe Failure(failure)
    calls.toSeq shouldBe Seq("state")
  }

  it should "preserve history on wallet failure or timeout and never return success" in {
    for (walletResult <- Seq(Future.successful(Failure(failure)), Future.failed[Try[Unit]](failure))) {
      val calls = ArrayBuffer.empty[String]
      val result = BlockchainReset.run(
        () => { calls += "state"; Success("state") },
        () => { calls += "wallet"; walletResult },
        () => { calls += "history"; Success("history") }
      )
      Await.result(result, 5.seconds) shouldBe Failure(failure)
      calls.toSeq shouldBe Seq("state", "wallet")
    }
  }

  it should "propagate both returned and thrown history storage errors" in {
    for (remove <- Seq[() => Try[String]](() => Failure(failure), () => throw failure)) {
      val result = BlockchainReset.run(
        () => Success("state"), () => Future.successful(Success(())), remove
      )
      Await.result(result, 5.seconds) shouldBe Failure(failure)
    }
  }
}
