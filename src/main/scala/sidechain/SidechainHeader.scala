package sidechain

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{R4, R5, R6, R7, R8}
import org.ergoplatform.mining.MainnetPoWVerifier
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.header.Header.Version
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}

import scala.util.Try
/**
  */
// todo: txs digest?

/**
  * @param ergoHeader
  * @param sidechainDigest - digest of AVL tree authenticating height -> sidechain header before this header
  * @param sideChainDataTxProof
  * @param sidechainTx
  * @param sidechainStateDigest
  */
case class SidechainHeader(ergoHeader: Header,
                           sidechainHeight: Int,
                           sidechainDigest: Array[Byte], // 33 bytes!
                           sideChainDataTxProof: TransactionMembershipProof,
                           sidechainTx: ErgoTransaction,
                           sidechainStateDigest: Array[Byte] // 33 bytes!
                          ) {

  val sidechainTxId: Array[Byte] = sidechainTx.serializedId

  val ergoHeaderId: Array[Version] = ergoHeader.serializedId

  /**
    * Cryptographically strong sidechain block (header) id
    */
  val serializedId: Digest32 = Algos.hash(sidechainDigest ++ sidechainTxId ++ ergoHeaderId ++ sidechainStateDigest)

  val id: ModifierId = bytesToId(serializedId)
}

case class SidechainBlock(header: SidechainHeader, transactions: IndexedSeq[ErgoTransaction])

trait SidechainDatabase {
  def currentHeight: Int
  def sidechainUtxoSetsAtHeight(height: Int): Array[ADDigest]
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

object SidechainHeader {

  val SideChainNFT: ModifierId = ModifierId @@ ""

  def generate(ergoHeader: Header,
               mainChainTx: ErgoTransaction,
               sidechainTxs: IndexedSeq[ErgoTransaction]): SidechainBlock = {
    ???
  }

  private def checkSidechainData(sidechainDataBox: ErgoBox, db: SidechainDatabase): Boolean = {
    val res = Try {
      // REGISTERS
      //  R4: (Long)         h       - Height of the sidechain.
      //  R5: (Coll[Byte])  T_h     - Digest of state changes (transactions) done at h.
      //  R6: (Coll[Byte])  U_h     - UTXO set digest after processing changes.
      //  R7: (Coll[Byte])  chainDigest  - AVL tree where leaf has height as key and hash of corresponding states hash(h, T_h, U_h, chainDigest_{h-1}) as value.
      //  R8: (Int) - height of the main-chain when side-chain was updated last time

      val regs = sidechainDataBox.additionalRegisters
      val h = regs(R4).value.asInstanceOf[Int]
      val tH = regs(R5)
      val uH = regs(R6).value.asInstanceOf[Array[Byte]]
      val cH = regs(R7)
      val lastUpdateHeight = regs(R8)

      val knownStateIds = db.sidechainUtxoSetsAtHeight(h)

      if (knownStateIds.find(_.sameElements(uH)).isDefined) {

      } else {

      }

      ???
    }
    res.isSuccess
  }

  /**
    * Function for sidechain validator to verify sidechain header
    */
  def verify(sh: SidechainHeader, db: SidechainDatabase): Boolean = {
    val txProof = sh.sideChainDataTxProof
    val sidechainDataBox = sh.sidechainTx.outputs.head
    MainnetPoWVerifier.validate(sh.ergoHeader).isSuccess &&  // check pow todo: lower diff
      txProof.valid(sh.ergoHeader.transactionsRoot) &&       // check sidechain tx membership
      txProof.txId == sh.sidechainTx.id &&                   // check provided sidechain is correct
      sidechainDataBox.tokens.contains(SideChainNFT) &&      // check that first output has sidechain data MFT
      checkSidechainData(sidechainDataBox, db)
    // todo: check sidechain data
    // todo: enforce linearity
    ???
  }

  def verify(sb: SidechainBlock, db: SidechainDatabase): Boolean = {
    verify(sb.header, db)
    // todo: verify transactions
  }

}
