package org.ergoplatform.settings

import scorex.util.ScorexLogging

import java.util.concurrent.atomic.AtomicReference

/**
  * Holds the current [[ErgoSettings]] in an `AtomicReference` so it can be
  * swapped at runtime. Reads via [[current]] are lock-free; swaps via
  * [[trySwap]] are serialized through a write lock so the persister cannot
  * interleave with another swap.
  *
  * On a successful swap the holder invokes the configured `onSwap` callback,
  * which production code uses to broadcast a `SettingsUpdated` event on the
  * actor-system event stream. The holder itself has no Akka dependency, so it
  * can be used in unit tests without an `ActorSystem`.
  */
class SettingsHolder(
  initial: ErgoSettings,
  persister: ErgoSettings => Either[PersistError, Unit],
  onSwap: (ErgoSettings, ErgoSettings) => Unit
) extends ScorexLogging {

  private val ref = new AtomicReference[ErgoSettings](initial)
  private val writeLock = new Object

  /** @return the latest committed settings snapshot. */
  def current: ErgoSettings = ref.get()

  /**
    * Atomically apply a candidate `ErgoSettings`:
    *   - persists via the configured persister
    *   - on persister success: swaps the ref and runs `onSwap(previous, current)`
    *   - on persister failure: leaves state unchanged, returns the `Left`
    */
  def trySwap(candidate: ErgoSettings): Either[PersistError, ErgoSettings] = writeLock.synchronized {
    val previous = ref.get()
    persister(candidate).map { _ =>
      ref.set(candidate)
      log.info("Settings updated via runtime config API")
      onSwap(previous, candidate)
      candidate
    }
  }
}

object SettingsHolder {

  /** Published on the actor-system event stream by production code after a successful swap. */
  final case class SettingsUpdated(previous: ErgoSettings, current: ErgoSettings)

  /**
    * A holder with no persistence and no notifications. Intended for tests and
    * any path where runtime swaps are not expected (calling [[trySwap]] returns
    * `Left(NoWritableConfig)`).
    */
  def readonly(initial: ErgoSettings): SettingsHolder =
    new SettingsHolder(
      initial,
      _ => Left(PersistError.NoWritableConfig),
      (_, _) => ()
    )
}

/**
  * Reasons a [[SettingsHolder]] swap may fail to persist. Sealed so consumers
  * (notably the REST API) can map each case to an HTTP status code exhaustively.
  */
sealed trait PersistError extends Product with Serializable {
  def message: String
}

object PersistError {

  /** Node was started without a `--config` / `-c` flag; persistence is unavailable. */
  case object NoWritableConfig extends PersistError {
    val message: String =
      "node was not started with --config / -c pointing to a writable HOCON file"
  }

  /**
    * The user's config file is present but cannot be rewritten in place
    * (does not exist, not writable, contains `include` directives, etc.).
    */
  final case class ConfigFileUnsupported(reason: String) extends PersistError {
    def message: String = reason
  }

  /** Actual I/O failure during read, write, or atomic move. */
  final case class IoFailure(detail: String) extends PersistError {
    def message: String = s"I/O failure while persisting config: $detail"
  }
}
