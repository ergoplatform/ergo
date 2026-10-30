package org.ergoplatform.it.util

import scala.concurrent.duration._

/** Tracks when an observed key last changed, so that a wait can fail once what it watches
  * has stopped moving instead of running into its outer deadline. The clock starts at the
  * first sample; a missing key (a failed sample) never counts as a change. */
final class StallWatch[K](now: () => Deadline = () => Deadline.now) {
  private var last: Option[K]              = None
  private var changedAt: Option[Deadline]  = None
  private var longestQuiet: FiniteDuration = Duration.Zero

  /** Records a sample and returns for how long the key has not changed. */
  def record(key: Option[K]): FiniteDuration = synchronized {
    val sampledAt = now()
    val since     = changedAt.getOrElse(sampledAt)
    if (last.isDefined) longestQuiet = longestQuiet.max(sampledAt - since)
    if (changedAt.isEmpty || (key.isDefined && key != last)) {
      changedAt = Some(sampledAt)
      last = key.orElse(last)
      Duration.Zero
    } else {
      sampledAt - since
    }
  }

  /** The longest time without a change since the first key: how close a wait came to
    * failing. */
  def longestQuietPeriod: FiniteDuration = synchronized(longestQuiet)
}

object StallWatch {

  /** A Younger or Equal peer is synced again only after ErgoSyncTracker's 60 s
    * SyncThreshold, plus a 5 s sync tick and a 10 s delivery timeout, so a node can
    * legitimately stand still for ~75 s; green CI runs stand still for at most ~30 s. */
  val DefaultLimit: FiniteDuration = 120.seconds
}

/** A wait gave up because what it watches stopped changing. Not a TimeoutException, which
  * ConvergenceObservations.until would replace with its deadline message. */
class NoProgressException(message: String) extends Exception(message)
