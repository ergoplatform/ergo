package org.ergoplatform.modifiers

import io.circe.Json
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.settings.Constants._
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block.Timestamp
import scorex.core.serialization.Serializer

//TODO we need it to be ErgoPersistentModifier just to put it to ProcessInfo
case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         aDProofs: Option[ADProof]) extends ErgoPersistentModifier {
  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.modifierTypeId

  override val parentId = header.parentId

  override lazy val id: ModifierId = Algos.hash(header.id ++ blockTransactions.id ++
    aDProofs.map(_.id).getOrElse(Array()))

  override lazy val json: Json = ???

  override type M = ErgoFullBlock

  override lazy val serializer: Serializer[ErgoFullBlock] = ???
}

object ErgoFullBlock {
  val modifierTypeId: ModifierTypeId = (-127).toByte


  //TODO testnet genesis?
  //todo: real definition of a genesis block, do we need genesis block at all?
  def genesisWithStateDigest(stateRoot: Digest): ErgoFullBlock = {
    val genesisTimestamp = 1500203225564L

    //todo: fix
    val genesisTx = new AnyoneCanSpendTransaction(
      IndexedSeq(0L),
      IndexedSeq(0L))
    //TODO where can we get it???
    val proofs = Array.fill(32)(0: Byte)
    val proofsRoot = ADProof.proofDigest(proofs)

    val header: Header = Header(0.toByte,
      Array.fill(hashLength)(0.toByte),
      Seq(),
      proofsRoot,
      stateRoot,
      BlockTransactions.rootHash(Seq(genesisTx.id)),
      genesisTimestamp,
      0,
      Array.fill(1)(0.toByte),
      Constants.InitialNBits,
      0,
      Array.fill(5)(0.toByte)
    )
    val blockTransactions: BlockTransactions = BlockTransactions(header.id, Seq(genesisTx))
    val aDProofs: ADProof = ADProof(header.id, proofs)

    //todo: fix
    assert(header.ADProofsRoot sameElements aDProofs.digest)
    assert(header.transactionsRoot sameElements blockTransactions.digest)
    ErgoFullBlock(header, blockTransactions, Some(aDProofs), None)
  }

  lazy val genesis: ErgoFullBlock = genesisWithStateDigest(Array.fill(32)(0:Byte))//ErgoState.afterGenesisStateDigest)
}
