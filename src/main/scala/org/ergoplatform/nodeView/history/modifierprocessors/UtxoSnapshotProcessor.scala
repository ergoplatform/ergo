package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.state.UtxoSnapshot
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Algos
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.util.ScorexLogging

import scala.util.{Failure, Success, Try}

/** Contains all functions for locally generated state snapshot processing.
  * */
trait UtxoSnapshotProcessor extends ScorexLogging with ScorexEncoding {

  protected val historyStorage: HistoryStorage

  def process(m: UtxoSnapshot): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(Algos.idToBAW(m.id), Seq.empty, m.chunks :+ m.manifest)
    ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
  }

  def validate(m: UtxoSnapshot): Try[Unit] = if (historyStorage.contains(m.id)) {
    Failure(new Exception(s"UtxoSnapshot with id ${m.encodedId} is already in history"))
  } else {
    Success(Unit)
  }

}
