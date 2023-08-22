package org.ergoplatform.modifiers.history.header

import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty


object WeakBlockAlgos {

  val weaksPerBlock = 128 // weak blocks per block

  def isWeak(header: Header, requiredDifficulty: Difficulty): Boolean = {
    val diff = requiredDifficulty / weaksPerBlock
    header.requiredDifficulty >= diff
  }



}
