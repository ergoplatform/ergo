package org.ergoplatform.settings

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.{ModifierTypeId, NodeViewModifier}


object Constants {
  val HashLength: Int = 32

  val CoinsInOneErgo: Long = 1000000000

  val MaxTarget: BigInt = BigInt(1, Array.fill(HashLength)((-1).toByte))
  val InitialDifficulty: Difficulty = BigInt(1)
  val InitialNBits: Long = RequiredDifficulty.encodeCompactBits(InitialDifficulty)
  val ModifierIdSize: Int = HashLength

  // Max cost of coinbase transaction. todo calculate? todo: do we need this constant
  val CoinbaseTxCost: Int = 10000

  val BlocksPerHour = 30

  //For how many blocks a box could be put into the state with no paying.
  //4 years
  val StoragePeriod: Int = 4 * 365 * 24 * BlocksPerHour

  val StorageContractCost: Long = 50

  val StorageIndexVarId: Byte = Byte.MaxValue

  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(Header.modifierTypeId -> HeaderSerializer,
      Extension.modifierTypeId -> ExtensionSerializer,
      BlockTransactions.modifierTypeId -> BlockTransactionsSerializer,
      ADProofs.modifierTypeId -> ADProofSerializer,
      Transaction.ModifierTypeId -> ErgoTransactionSerializer)
}
