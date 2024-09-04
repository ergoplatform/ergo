package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.subblocks.SubBlockInfo

trait SubBlocksProcessor {

  // sub-blocks related logic
  def applySubBlockHeader(sbi: SubBlockInfo): Unit = {
    // todo: implement
  }
}
