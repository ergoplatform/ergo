package sidechain

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeContext}
import org.ergoplatform.ErgoBox.{R4, R5, R6, R7, R8}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.mining.MainnetPoWVerifier
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.header.Header.Version
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.sdk.BlockchainParameters
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ScorexEncoder
import org.ergoplatform.validation.ValidationResult.Valid
import org.ergoplatform.validation.{InvalidModifier, ModifierValidator, ValidationResult, ValidationSettings, ValidationState}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.authds.{ADDigest, LeafData}
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values
import sigmastate.Values.AvlTreeConstant
import sigmastate.eval.CAvlTree
import sigmastate.interpreter.Interpreter.ScriptEnv

import scala.util.{Failure, Success, Try}

/**
  * @param ergoHeader           - mainchain header
  * @param sidechainDigest      - digest of AVL tree authenticating height -> sidechain header before this header
  * @param sideChainDataTxProof - proof of inclusion for transaction carrying sidechain data
  * @param sidechainTx          - mainchain tx containing sidechain data
  * @param sidechainStateDigest - digest of sidechain's UTXO set
  */
case class SidechainHeader(ergoHeader: Header,
                           sidechainHeight: Int,
                           sidechainDigest: Array[Byte], // 33 bytes!
                           sideChainDataTxProof: TransactionMembershipProof,
                           sidechainTx: ErgoTransaction,
                           sidechainStateDigest: Array[Byte] // 33 bytes!
                          ) {

  lazy val sidechainDataBox: ErgoBox = sidechainTx.outputs.head

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
  def currentSidechainHeight(): Int

  def sidechainUtxoSetsAtHeight(height: Int): Array[ADDigest]

  def updateMainchainCommitment(sidechainHeader: SidechainHeader): Unit

  def currentMainchainCommitment(): SidechainHeader

  def isInSameSidechain(sh1: SidechainHeader, sh2: SidechainHeader): Boolean

  def utxoSetFor(digest: ADDigest): UtxoState

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

  val SidechainHeaderModifierTypeId: NetworkObjectTypeId.Value = NetworkObjectTypeId.Value @@ 64.toByte

  val SideChainNFT: ModifierId = ModifierId @@ ""

  def generate(ergoHeader: Header,
               mainChainTx: ErgoTransaction,
               sidechainTxs: IndexedSeq[ErgoTransaction]): SidechainBlock = {

    val txIds: Seq[Array[Byte]] = sidechainTxs.map(_.serializedId)
    val witnessIds: Seq[Array[Byte]] = sidechainTxs.map(tx => tx.witnessSerializedId)
    val txsDigest: MerkleTree[Digest32] = Algos.merkleTree(LeafData @@ (txIds ++ witnessIds))

    ???
  }

  sealed trait SidechainBlockValidationResult {
    def isValid: Boolean

    def nextCheck(check: => SidechainBlockValidationResult): SidechainBlockValidationResult = {
      if (isValid) check else this
    }
  }

  case object AllIsOkay extends SidechainBlockValidationResult {
    override def isValid: Boolean = true
  }

  trait Invalid extends SidechainBlockValidationResult {
    def message: String
    override def isValid: Boolean = false
  }

  case class InvalidPow(cause: Throwable) extends Invalid {
    override def message: String = s"Mainchain block header failed PoW validation due to: $cause"
  }

  case object InvalidTxProof extends Invalid {
    override def message = "Invalid Merkle proof for sidechain data transaction"
  }

  case object InvalidTxId extends Invalid {
    override def message = "Invalid transaction id in Markle proof"
  }

  case object InvalidSidechainNFT extends Invalid {
    override def message = "No sidechain NFT found in mainchain transaction"
  }

  trait SidechainHeaderStatus

  /**
    * Validation result for sidechain data written on the mainchain
    */
  trait MainchainDataValidationResult extends SidechainBlockValidationResult {
    def headerChainStatus: SidechainHeaderStatus
  }

  case object Ahead extends SidechainHeaderStatus

  case object Behind extends SidechainHeaderStatus

  case object BetterFork extends SidechainHeaderStatus

  case object NonBetterFork extends SidechainHeaderStatus

  case object Unknown extends SidechainHeaderStatus


  case class UnknownBlockCommitted(chainDigest: Array[Byte],
                                   stateDigest: Array[Byte]) extends MainchainDataValidationResult {

    override def headerChainStatus: SidechainHeaderStatus = Unknown

    override def isValid: Boolean = false

  }


  case class MainchainSidechainDataValid(override val headerChainStatus: SidechainHeaderStatus)
    extends MainchainDataValidationResult {
    override def isValid: Boolean = true
  }

  case class MainchainSidechainDataInvalid(cause: Throwable)
    extends MainchainDataValidationResult {
    override def isValid: Boolean = true
  }

  private def checkSidechainData(sidechainHeader: SidechainHeader,
                                 db: SidechainDatabase): MainchainDataValidationResult = {
    Try {
      val sidechainDataBox = sidechainHeader.sidechainDataBox

      // REGISTERS
      //  R4: (Long)         h       - Height of the sidechain.
      //  R5: (Coll[Byte])  T_h     - Digest of state changes (transactions) done at h.
      //  R6: (Coll[Byte])  U_h     - UTXO set digest after processing changes.
      //  R7: (Coll[Byte])  chainDigest  - AVL tree where leaf has height as key and hash of corresponding states hash(h, T_h, U_h, chainDigest_{h-1}) as value.
      //  R8: (Int) - height of the main-chain when side-chain was updated last time

      val regs = sidechainDataBox.additionalRegisters
      val h = regs(R4).value.asInstanceOf[Int]
      val txsDigest = regs(R5).value.asInstanceOf[Array[Byte]]
      val stateDigest = regs(R6).value.asInstanceOf[Array[Byte]]
      val chainDigest = regs(R7).value.asInstanceOf[Array[Byte]] // todo: check correctness
      val lastUpdateHeight = regs(R8).asInstanceOf[Int] // todo: check against current mainchain height

      val knownStateIds = db.sidechainUtxoSetsAtHeight(h)

      val currentCmt = db.currentMainchainCommitment()

      /**
        * We check if we mainchain is committing to known sidechain block.
        * If so, we check if this block ahead, behind or in fork.
        */
      if (knownStateIds.exists(_.sameElements(stateDigest))) {
        val sameChain = db.isInSameSidechain(currentCmt, sidechainHeader)

        if (sameChain && sidechainHeader.sidechainHeight > currentCmt.sidechainHeight) {
          Ahead
        } else if (sameChain && sidechainHeader.sidechainHeight < currentCmt.sidechainHeight) {
          Behind
        } else {
          Fork
        }
      } else {
        UnknownBlockCommitted(chainDigest, stateDigest)
      }
    }.getOrElse(MainchainDataValidationError()) // todo: pass error
  }


  /**
    * Function for sidechain validator to verify sidechain header
    */
  def verify(sh: SidechainHeader, db: SidechainDatabase): ValidationResult[SidechainHeaderStatus] = {
    val txProof = sh.sideChainDataTxProof

    def checkPow: SidechainBlockValidationResult = {
      MainnetPoWVerifier.validate(sh.ergoHeader) match {
        case Success(_) => AllIsOkay
        case Failure(e) => InvalidPow(e)
      }
    }

    def checkTxProof: SidechainBlockValidationResult = {
      if(txProof.valid(sh.ergoHeader.transactionsRoot)){
        AllIsOkay
      } else {
        InvalidTxProof
      }
    }

    def checkTxId: SidechainBlockValidationResult = {
      if(txProof.txId == sh.sidechainTx.id){
        AllIsOkay
      } else {
        InvalidTxId
      }
    }

    def checkSidechainNFT = {
      if(sh.sidechainDataBox.tokens.contains(SideChainNFT)) {
        AllIsOkay
      } else {
        InvalidSidechainNFT
      }
    }

    AllIsOkay    // start with OK state
      .nextCheck(checkPow)
      .nextCheck(checkTxProof)
      .nextCheck(checkTxId)
      .nextCheck(checkSidechainNFT)
      .nextCheck(checkSidechainData(sh, db))

    // todo: additional checks to enforce linearity
    ???
  }

  def verify(sb: SidechainBlock, db: SidechainDatabase): ValidationResult[SidechainHeaderStatus] = {
    verify(sb.header, db)
    // todo: verify transactions
  }

  def process(sb: SidechainBlock, db: SidechainDatabase): Unit = {
    verify(sb, db) match {
      case ValidationResult.Valid(_) => ???
      case ValidationResult.Invalid(_) => ???
    }
  }

}


class SidechainInterpreter(params: BlockchainParameters) extends ErgoInterpreter(params) {
  val UtxoStateVarId = 125.toByte

  // storage rent turned off
  override def checkExpiredBox(box: ErgoBox, output: ErgoBoxCandidate, currentHeight: Height): Boolean = {
    false
  }

  override def verify(env: ScriptEnv,
                      exp: Values.ErgoTree,
                      context: ErgoLikeContext,
                      proof: Array[Version],
                      message: Array[Version]): Try[(Boolean, Long)] = {
    val extendedCtx = context.withExtension(context.extension.add(UtxoStateVarId -> AvlTreeConstant(CAvlTree(context.lastBlockUtxoRoot))))
    super.verify(env, exp, extendedCtx, proof, message)
  }

}
