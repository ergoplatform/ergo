package org.ergoplatform.settings

import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionSerializer}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import scorex.core.NodeViewModifier
import scorex.core.serialization.ErgoSerializer
import scorex.crypto.authds.avltree.batch.AvlTreeParameters
import sigmastate.Values
import sigmastate.Values.ErgoTree


object Constants {
  /**
    * Length of hash function output used around the Ergo code
    */
  val HashLength: Int = scorex.crypto.authds.avltree.batch.Constants.HashLength

  val CoinsInOneErgo: Long = 1000000000

  val MaxTarget: BigInt = BigInt(1, Array.fill(HashLength)((-1).toByte))
  val InitialDifficulty: Difficulty = BigInt(1)
  val InitialNBits: Long = DifficultySerializer.encodeCompactBits(InitialDifficulty)
  val ModifierIdSize: Int = HashLength

  val BlocksPerHour = 30

  val BlocksPerDay: Int = BlocksPerHour * 24

  val BlocksPerWeek: Int = BlocksPerDay * 7

  val BlocksPerMonth: Int = BlocksPerDay * 30

  val BlocksPerYear: Int = BlocksPerDay * 365

  //For how many blocks a box could be put into the state with no paying.
  //4 years
  val StoragePeriod: Int = 4 * BlocksPerYear

  val StorageContractCost: Long = 50

  val StorageIndexVarId: Byte = Byte.MaxValue

  // Number of last block headers available is scripts from ErgoStateContext
  val LastHeadersInContext = 10

  /**
    * Serializers for block sections and transactions
    *
    * // todo: move to NodeViewSynchronizer, used only there
    */
  val modifierSerializers: Map[NetworkObjectTypeId.Value, ErgoSerializer[_ <: NodeViewModifier]] =
    Map(Header.modifierTypeId -> HeaderSerializer,
      Extension.modifierTypeId -> ExtensionSerializer,
      BlockTransactions.modifierTypeId -> BlockTransactionsSerializer,
      ADProofs.modifierTypeId -> ADProofsSerializer,
      ErgoTransaction.modifierTypeId -> ErgoTransactionSerializer)

  val SoftForkEpochs = 32 //about 45.5 days

  def TrueLeaf: ErgoTree = Values.TrueLeaf.toSigmaProp
  def FalseLeaf: ErgoTree = Values.FalseLeaf.toSigmaProp

  val StringEncoding = "UTF-8"

  // Maximum extension size
  val MaxExtensionSize: Int = 32 * 1024

  // Maximum extension size during bytes parsing
  val MaxExtensionSizeMax: Int = 1024 * 1024

  /**
    * UTXO set snapshot to be taken every this number of blocks.
    * The value MUST be divisible by voting epoch length (chainSettings.voting.votingLength),
    * so after snapshot the state is corresponding to a moment before applying first block of a voting epoch,
    * and then the first block sets current validation parameters.
    *
    * So for the Ergo mainnet the value should be divisible by 1024 (for testnet, 128, any number divisible by
    * 1024 is divisible by 128 also.
    */
  // todo: change before deployment to 51200
  val MakeSnapshotEvery = 500

  /**
    * AVL+ tree node parameters. The tree is used to authenticate UTXO set.
    * Keys and hashes are 256-bits long, values are boxes, so value size is dynamic.
    */
  object StateTreeParameters extends AvlTreeParameters(keySize = HashLength, valueSize = None, labelSize = HashLength)
}
