package sidechain

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof

/**
  */
case class MMSidechainHeader(ergoHeader: Header,
                             sideChainDataProof: TransactionMembershipProof,
                             sidechainTx: ErgoTransaction)


object MMSidechainHeader {

  def verify(sh: MMSidechainHeader): Boolean = {
    //todo: enforce linearity
    ???
  }
}
