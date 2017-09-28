package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendNoncedBoxSerializer}
import org.ergoplatform.settings.Algos
import scorex.core.VersionTag
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation.
  *
  * @param store - database where persistent UTXO set authenticated with the help of an AVL+ tree is residing
  */
class UtxoState(override val version: VersionTag, val store: Store)
  extends ErgoState[UtxoState] {
  import UtxoState.metadata

  implicit val hf = new Blake2b256Unsafe
  private lazy val np = NodeParameters(keySize = 32, valueSize = ErgoState.BoxSize, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize)), storage
    ).get

  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    IndexedSeq(AnyoneCanSpendNoncedBox(height, height))
  }

  //TODO not efficient at all
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): Try[(SerializedAdProof, ADDigest)] = Try {
    require(persistentProver.digest.sameElements(rootHash))
    require(storage.version.get.sameElements(rootHash))
    require(store.lastVersionID.get.data.sameElements(rootHash))

    persistentProver.checkTree(true)

    val mods = boxChanges(txs).operations.map(ADProofs.changeToMod)
    mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
      t.flatMap(_ => {
        val opRes = persistentProver.performOneOperation(m)
        if (opRes.isFailure) println(opRes)
        opRes
      })
    }.ensuring(_.isSuccess)

    val proof = persistentProver.generateProofAndUpdateStorage()

    val digest = persistentProver.digest

    persistentProver.checkTree(true)

    persistentProver.rollback(rootHash).ensuring(persistentProver.digest.sameElements(rootHash))

    persistentProver.checkTree(true)

    proof -> digest
  }

  override val rootHash: ADDigest = persistentProver.digest

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    val p = persistentProver
    val hash = ADDigest @@ store.get(ByteArrayWrapper(version)).get.data
    p.rollback(hash).map { _ =>
      new UtxoState(version, store) {
        override protected lazy val persistentProver = p
      }
    }
  }

  //todo: don't use assert
  private[state] def checkTransactions(transactions: Seq[AnyoneCanSpendTransaction], expectedDigest: ADDigest) = Try {

    transactions.foreach(tx => assert(tx.semanticValidity.isSuccess))

    val mods = boxChanges(transactions).operations.map(ADProofs.changeToMod)
    mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
      t.flatMap(_ => {
        persistentProver.performOneOperation(m)
      })
    }.ensuring(_.isSuccess)

    assert(expectedDigest.sameElements(persistentProver.digest), "digest after txs application is wrong")
  }

  //todo: utxo snapshot could go here
  //todo: dont' use assert
  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] = mod match {
    case fb: ErgoFullBlock =>

      //todo: rollback if failure on the way
      checkTransactions(fb.blockTransactions.txs, fb.header.stateRoot) match {
        case Success(_) =>
          Try {
            val md = metadata(VersionTag @@ fb.id, fb.header.stateRoot)
            val proofBytes = persistentProver.generateProofAndUpdateStorage(md)
            val proofHash = ADProofs.proofDigest(proofBytes)
            assert(fb.header.ADProofsRoot.sameElements(proofHash))
            new UtxoState(VersionTag @@ fb.id, store)
          }
        case Failure(e) =>
          log.warn(s"Error while applying a modifier ${mod.id}: ", e)
          Failure(e)
      }

    case a: Any =>
      log.info(s"Unhandled modifier: $a")
      Failure(new Exception("unknown modifier"))
  }

  def boxById(id: ADKey): Option[AnyoneCanSpendNoncedBox] =
    persistentProver
      .unauthenticatedLookup(id)
      .map(AnyoneCanSpendNoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  override def rollbackVersions: Iterable[VersionTag] =
    persistentProver.storage.rollbackVersions.map(v => VersionTag @@ store.get(ByteArrayWrapper(Algos.hash(v))).get.data)
}

object UtxoState {
  private lazy val bestVersionKey = Algos.hash("best state version")

  private def metadata(modId: VersionTag, stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
  }


  //todo: check database state, read version from it
  def create(versionTag: Option[VersionTag], dir: File): UtxoState = {
    val store = new LSMStore(dir, keepVersions = 20) // todo: magic number, move to settings
    new UtxoState(versionTag.getOrElse(ErgoState.genesisStateVersion), store)
  }

  def fromBoxHolder(bh: BoxHolder, dir: File): UtxoState = {
    val p = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize))
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val store = new LSMStore(dir, keepVersions = 20) // todo: magic number, move to settings

    new UtxoState(ErgoState.genesisStateVersion, store) {
      override protected lazy val persistentProver =
        PersistentBatchAVLProver.create(
          p,
          storage,
          metadata(ErgoState.genesisStateVersion, p.digest),
          paranoidChecks = true
        ).get

      assert(persistentProver.digest.sameElements(storage.version.get))
    }
  }
}