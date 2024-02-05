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
// todo: txs digest?

case class SidechainHeader(ergoHeader: Header,
                           prevSidechainHeaderId: Array[Byte],
                           sideChainDataTxProof: TransactionMembershipProof,
                           sidechainTx: ErgoTransaction,
                           sidechainStateDigest: Array[Byte] // 33 bytes!
                          ) {

  val sidechainTxId: Array[Byte] = sidechainTx.serializedId

  val ergoHeaderId: Array[Version] = ergoHeader.serializedId

  val serializedId: Digest32 = Algos.hash(prevSidechainHeaderId ++ sidechainTxId ++ ergoHeaderId ++ sidechainStateDigest)

  val id: ModifierId = bytesToId(serializedId)
}

case class SidechainBlock(header: SidechainHeader, transactions: IndexedSeq[ErgoTransaction])

/**
  *
  * Plan to implement simplest sidechain, no additional functionality aside of supporting context ext variable with
  * special id:
  * * block header structure
  * * generation and verification
  * * sidechain contracts deployment
  * * simulation of transfers
  */

object SidechainHeader {

  val SideChainNFT: ModifierId = ModifierId @@ ""

  def generate(ergoHeader: Header,
               mainChainTx: ErgoTransaction,
               sidechainTxs: IndexedSeq[ErgoTransaction]): SidechainBlock = {
      ???
  }

  def verify(sh: SidechainHeader): Boolean = {
    val txProof = sh.sideChainDataTxProof
    val sidechainDataBox = sh.sidechainTx.outputs.head
    MainnetPoWVerifier.validate(sh.ergoHeader).isSuccess &&  // check pow todo: lower diff
      txProof.valid(sh.ergoHeader.transactionsRoot) &&       // check sidechain tx membership
      txProof.txId == sh.sidechainTx.id &&                   // check provided sidechain is correct
      sidechainDataBox.tokens.contains(SideChainNFT)         // check that first output has sidechain data MFT
    // todo: check sidechain data
    //todo: enforce linearity
    ???
  }

}
