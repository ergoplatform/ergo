package sidechain

import org.ergoplatform.mining.MainnetPoWVerifier
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.header.Header.Version
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
import org.ergoplatform.settings.Algos
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}

/**
  */
// todo: link to prev
case class MMSidechainHeader(ergoHeader: Header,
                             prevSidechainHeaderId: Array[Byte],
                             sideChainDataProof: TransactionMembershipProof,
                             sidechainTx: ErgoTransaction) {

  val sidechainTxId: Array[Byte] = sidechainTx.serializedId

  val ergoHeaderId: Array[Version] = ergoHeader.serializedId

  val serializedId: Digest32 = Algos.hash(prevSidechainHeaderId ++ sidechainTxId ++ ergoHeaderId)

  val id: ModifierId = bytesToId(serializedId)
  
}

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
