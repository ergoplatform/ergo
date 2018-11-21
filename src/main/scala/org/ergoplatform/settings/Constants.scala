package org.ergoplatform.settings

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.{MinerPubkey, Outputs}
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.{ModifierTypeId, NodeViewModifier}
import sigmastate.Values.{IntConstant, Value}
import sigmastate._
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.utxo.{ByIndex, ExtractScriptBytes, SizeOf}


object Constants {
  val HashLength: Int = 32

  val CoinsInOneErgo: Long = 1000000000

  val MaxTarget: BigInt = BigInt(1, Array.fill(HashLength)((-1).toByte))
  val InitialDifficulty: Difficulty = BigInt(1)
  val InitialNBits: Long = RequiredDifficulty.encodeCompactBits(InitialDifficulty)
  val ModifierIdSize: Int = HashLength

  val BlocksPerHour = 30

  val BlocksPerDay = BlocksPerHour * 24

  val BlocksPerWeek = BlocksPerDay * 7

  val BlocksPerMonth = BlocksPerDay * 30

  val BlocksPerYear = BlocksPerDay * 365

  //For how many blocks a box could be put into the state with no paying.
  //4 years
  val StoragePeriod: Int = 4 * 365 * 24 * BlocksPerHour

  val StorageContractCost: Long = 50

  val StorageIndexVarId: Byte = Byte.MaxValue
  
  // Number of last block headers available is scripts from ErgoStateContext
  val LastHeadersInContext = 10

  val FeeProposition: Value[SBoolean.type] = {
    val correctMinerProposition = EQ(
      ExtractScriptBytes(ByIndex(Outputs, IntConstant(0))),
      ErgoTreeSerializer.serializedPubkeyPropValue(MinerPubkey)
    )
    val outputsNum = EQ(SizeOf(Outputs), 1)
    AND(correctMinerProposition, outputsNum)
  }

  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(Header.modifierTypeId -> HeaderSerializer,
      Extension.modifierTypeId -> ExtensionSerializer,
      BlockTransactions.modifierTypeId -> BlockTransactionsSerializer,
      ADProofs.modifierTypeId -> ADProofSerializer,
      Transaction.ModifierTypeId -> ErgoTransactionSerializer)
}
