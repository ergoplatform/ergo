package org.ergoplatform.nodeView

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/** Sequences a reset without deleting recovery history before both rollback operations succeed.
  * This is not a cross-database transaction: callers must stop processing after any failure
  * because a failed storage operation may already have changed persistent data.
  */
private[nodeView] object BlockchainReset {
  private def marker(directory: String): Path = Paths.get(directory, "history-reset.incomplete")

  /** Refuse startup after interrupted pruning, rather than treating a partial history as empty. */
  def requireCompleteHistory(directory: String): Unit = {
    require(!Files.exists(marker(directory)),
      "An incomplete blockchain history reset was detected. Restore a consistent backup or resync before restarting.")
  }

  /** Persist intent before destructive pruning; keep it after any failure for restart protection. */
  def pruneHistory[H](directory: String, height: Int)(operation: => Try[H]): Try[H] = Try {
    val intent = marker(directory)
    Files.write(intent, s"targetHeight=$height\n".getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE, StandardOpenOption.SYNC)
    val history = operation.get
    Files.delete(intent)
    history
  }

  /** Reject unavailable target versions before invoking any mutating operation. */
  def validateTarget(height: Int, currentHeight: Int, hasFullBlock: Boolean,
                     canRollback: Boolean): Try[Unit] = Try {
    require(height > 0, "Reset height must be positive; resetting to genesis requires a full resync")
    require(height <= currentHeight, s"Cannot reset to height $height, higher than current height $currentHeight")
    require(hasFullBlock, s"No full block found at reset height $height")
    require(canRollback, s"State rollback version at height $height is unavailable")
  }

  /** Wait for the wallet's persisted rollback acknowledgement before pruning history. */
  def run[S, H](rollbackState: () => Try[S],
                rollbackWallet: () => Future[Try[Unit]],
                resetHistory: () => Try[H])
               (implicit ec: ExecutionContext): Future[Try[(S, H)]] = {
    val reset = for {
      state <- Future.fromTry(Try(rollbackState()).flatten)
      _ <- Try(rollbackWallet()).fold(Future.failed, identity).flatMap(result => Future.fromTry(result))
      history <- Future.fromTry(Try(resetHistory()).flatten)
    } yield (state, history)
    reset.map(result => Success(result): Try[(S, H)]).recover {
      case NonFatal(error) => Failure(error)
    }
  }
}
