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
import org.ergoplatform.sdk.BlockchainParameters
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ScorexEncoder
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

import scala.util.Try

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

  trait ProcessingCommand

  trait SidechainDataValidationResult

  case object Ahead extends SidechainDataValidationResult

  case object Behind extends SidechainDataValidationResult

  case object Fork extends SidechainDataValidationResult

  case class UnknownBlockCommitted(chainDigest: Array[Byte],
                                   stateDigest: Array[Byte]) extends SidechainDataValidationResult

  case class SidechainDataValidationError() extends SidechainDataValidationResult

  private def checkSidechainData(sidechainHeader: SidechainHeader,
                                 db: SidechainDatabase): SidechainDataValidationResult = {
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
    }.getOrElse(SidechainDataValidationError()) // todo: pass error
  }


  object SidechainValidationSettings extends ValidationSettings {
    override val isFailFast: Boolean = true

    // todo: move out of ValidationSettings
    override def getError(id: Short, invalidMod: InvalidModifier): ValidationResult.Invalid = ???

    // todo: move out of ValidationSettings
    override def isActive(id: Short): Boolean = ???
  }

  /**
    * Function for sidechain validator to verify sidechain header
    */
  def verify(sh: SidechainHeader, db: SidechainDatabase): ValidationResult[ProcessingCommand] = {
    val txProof = sh.sideChainDataTxProof

    val vs = ValidationState(ModifierValidator.success, SidechainValidationSettings)(ScorexEncoder.default)

    val validateBeforeSidechainData =
      vs.validateNoFailure(1.toShort, MainnetPoWVerifier.validate(sh.ergoHeader), NetworkObjectTypeId.Value @@ 64.toByte) // todo: lower diff
        .validate(2, txProof.valid(sh.ergoHeader.transactionsRoot), InvalidModifier("" , sh.id, SidechainHeaderModifierTypeId)) // check sidechain tx membership
        .validate(3, txProof.txId == sh.sidechainTx.id, InvalidModifier("" , sh.id, SidechainHeaderModifierTypeId)) // check provided sidechain is correct
        .validate(4, sh.sidechainDataBox.tokens.contains(SideChainNFT), InvalidModifier("" , sh.id, SidechainHeaderModifierTypeId))// check that first output has sidechain data MFT

    // todo: fix
    validateBeforeSidechainData.validate(5, checkSidechainData(sh, db).isInstanceOf[Ahead.type], InvalidModifier("" , sh.id, SidechainHeaderModifierTypeId)) // check sidechain data committed in the main-chain

    // todo: enforce linearity
    ???
  }

  def verify(sb: SidechainBlock, db: SidechainDatabase): ValidationResult[ProcessingCommand] = {
    verify(sb.header, db)
    // todo: verify transactions
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
