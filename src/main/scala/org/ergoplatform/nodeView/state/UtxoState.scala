package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.ADProof
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendNoncedBoxSerializer}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.crypto.authds.avltree.AVLValue
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.Blake2b256Unsafe

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation.
  *
  * @param dir - folder where persistent UTXO set authenticated with the help of an AVL+ tree is located
  */
class UtxoState(dir: File) extends ErgoState[UtxoState] {

  implicit val hf = new Blake2b256Unsafe

  private val store = new LSMStore(dir, keepVersions = 20) // todo: magic number, fix
  private val np = NodeParameters(keySize = 32, valueSize = ErgoState.BoxSize, labelSize = 32)
  protected val storage = new VersionedIODBAVLStorage(store, np)

  protected lazy val persistentProver =
    PersistentBatchAVLProver.create(new BatchAVLProver(keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize)), storage).get

  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    IndexedSeq(AnyoneCanSpendNoncedBox(height, height))
  }

  //TODO not efficient at all
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): Try[(ADProof.ProofRepresentation, Digest)] = Try {
    require(persistentProver.digest.sameElements(rootHash))
    require(storage.version.get.sameElements(rootHash))
    require(store.lastVersionID.get.data.sameElements(rootHash))

    persistentProver.checkTree(true)

    val mods = boxChanges(txs).operations.map(ADProof.changeToMod)
    mods.foldLeft[Try[Option[AVLValue]]](Success(None)) { case (t, m) =>
      t.flatMap(_ => {
        persistentProver.performOneOperation(m)
      })
    }.ensuring(_.isSuccess)

    val proof = persistentProver.generateProof
    val digest = persistentProver.digest

    persistentProver.checkTree(true)

    persistentProver.rollback(rootHash).ensuring(persistentProver.digest.sameElements(rootHash))

    persistentProver.checkTree(true)

    proof -> digest
  }

  override val rootHash: Digest = persistentProver.digest

  //todo: the same question as with DigestState.version
  override def version: VersionTag = rootHash

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    val p = persistentProver
    p.rollback(version).map { _ =>
      new UtxoState(dir) {
        override protected lazy val persistentProver = p
      }
    }
  }

  override def validate(mod: ErgoPersistentModifier): Try[Unit] =
    Failure(new Exception("validate() is not implemented for UtxoState as it requires for costly provers' rollback"))

  //todo: don't use assert
  private[state] def checkTransactions(transactions: Seq[AnyoneCanSpendTransaction], expectedDigest: Digest) = Try {

    transactions.foreach(tx => assert(tx.semanticValidity.isSuccess))

    val mods = boxChanges(transactions).operations.map(ADProof.changeToMod)
    mods.foldLeft[Try[Option[AVLValue]]](Success(None)) { case (t, m) =>
      t.flatMap(_ => {
        persistentProver.performOneOperation(m)
      })
    }.ensuring(_.isSuccess)

    assert(expectedDigest.sameElements(persistentProver.digest), "digest after txs application is wrong")
  }

  //todo: utxo snapshot could go here
  //todo: dont' use assert
  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] =
  mod match {
    case fb: ErgoFullBlock =>
      checkTransactions(fb.blockTransactions.txs, fb.header.stateRoot).map { _ =>
        val proofBytes: Array[Byte] = persistentProver.generateProof
        val proofHash = ADProof.proofDigest(proofBytes)
        assert(fb.header.ADProofsRoot.sameElements(proofHash))
        new UtxoState(dir)
      }

    case a: Any =>
      log.info(s"Unhandled modifier: $a")
      Failure(new Exception("unknown modifier"))
  }

  def boxById(id: Digest): Option[AnyoneCanSpendNoncedBox] =
    persistentProver
      .unauthenticatedLookup(id)
      .map(AnyoneCanSpendNoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)
}

object UtxoState {
  def fromBoxHolder(bh: BoxHolder, dir: File): UtxoState = {
    val p = new BatchAVLProver(keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize))
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, b.bytes)).ensuring(_.isSuccess))

    new UtxoState(dir) {
      override protected lazy val persistentProver = PersistentBatchAVLProver.create(p, storage).get

      assert(persistentProver.digest.sameElements(storage.version.get))
      persistentProver.checkTree(true)
    }
  }
}
