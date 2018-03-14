package org.ergoplatform.bench

import org.ergoplatform.mining.FakePowScheme
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty

object FakePowForBench extends FakePowScheme(None) {
  override def realDifficulty(header: Header): Difficulty = header.requiredDifficulty
}
