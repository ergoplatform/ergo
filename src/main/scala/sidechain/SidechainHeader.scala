package sidechain

import org.ergoplatform.mining.MainnetPoWVerifier
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof

/**
  */
// todo: link to prev
case class MMSidechainHeader(ergoHeader: Header,
                             sideChainDataProof: TransactionMembershipProof,
                             sidechainTx: ErgoTransaction)

/**
  *
  * Plan to implement simplest sidechain, no additional functionality aside of supporting context ext variable with
  * special id:
  * * block header structure
  * * generation and verification
  * * sidechain contracts deployment
  * * simulation of transfers
  */

object MMSidechainHeader {

  def verify(sh: MMSidechainHeader): Boolean = {
    MainnetPoWVerifier.validate(sh.ergoHeader).isSuccess &&
      sh.sideChainDataProof.proof.valid(sh.ergoHeader.transactionsRoot)
    //todo: enforce linearity
    ???
  }
}
