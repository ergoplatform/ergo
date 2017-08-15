package org.ergoplatform.nodeView.state

import java.io.File

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProof.ProofRepresentation
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendNoncedBoxSerializer, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.{BoxStateChanges, Insertion, MinimalState, Removal}
import scorex.core.utils.ScorexLogging

import scala.util.Try


/**
  * Implementation of minimal state concept in Scorex. Minimal state (or just state from now) is some data structure
  * enough to validate a new blockchain element(e.g. block).
  * State in Ergo could be UTXO, like in Bitcoin or just a single digest. If the state is about UTXO, transaction set
  * of a block could be verified with no help of additional data. If the state is about just a digest, then proofs for
  * transformations of UTXO set presented in form of authenticated dynamic dictionary are needed to check validity of
  * a transaction set (see https://eprint.iacr.org/2016/994 for details).
  */
trait ErgoState[IState <: MinimalState[AnyoneCanSpendProposition.type,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  IState]] extends MinimalState[AnyoneCanSpendProposition.type,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  IState] with ScorexLogging {

  self: IState =>

  //TODO: kushti: AVL+ tree root, not Merkle
  def rootHash(): Digest

  //TODO implement correctly
  def stateHeight: Int = 0

  /**
    * Extract ordered sequence of operations on UTXO set from set of transactions
    */
  def boxChanges(txs: Seq[AnyoneCanSpendTransaction]): BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox] =
  BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](txs.flatMap { tx =>
    tx.boxIdsToOpen.map(id => Removal[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](id)) ++
      tx.newBoxes.map(b => Insertion[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](b))
  })

  override def version: VersionTag

  override def validate(mod: ErgoPersistentModifier): Try[Unit]

  override def applyModifier(mod: ErgoPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  override type NVCT = this.type
}

object ErgoState {

  type Digest = Array[Byte]

  val BoxSize = AnyoneCanSpendNoncedBoxSerializer.Length

  val initialDigest: Digest = Array.fill(32)(0: Byte)
  val genesisProofs: ProofRepresentation = Array.fill(32)(0: Byte)

  def readOrGenerate(settings: ErgoSettings) = {
    val stateDir = new File(s"${settings.dataDir}/state")
    stateDir.mkdirs()
    //TODO read from file if state already exists
    if (settings.ADState) new DigestState(initialDigest) else new UtxoState(initialDigest, stateDir)
  }
}
