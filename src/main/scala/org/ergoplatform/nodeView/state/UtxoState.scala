package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, QuickStore, Store}
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendNoncedBoxSerializer, AnyoneCanSpendProposition}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos
import scorex.core.VersionTag
import scorex.core.transaction.state.TransactionValidation
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation.
  *
  * @param store - database where persistent UTXO set authenticated with the help of an AVL+ tree is residing
  */
class UtxoState(override val version: VersionTag, val store: Store)
  extends ErgoState[UtxoState] with TransactionValidation[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction] {

  import UtxoState.metadata

  implicit val hf = new Blake2b256Unsafe
  private lazy val np = NodeParameters(keySize = 32, valueSize = ErgoState.BoxSize, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize)), storage
    ).get

  override val maxRollbackDepth = 10

  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    //TODO fix
    randomBox().toIndexedSeq
  }

  //TODO not efficient at all
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): Try[(SerializedAdProof, ADDigest)] = {

    def rollback(): Try[Unit] = Try(
      persistentProver.rollback(rootHash).ensuring(_.isSuccess && persistentProver.digest.sameElements(rootHash))
    ).flatten

    Try {
      require(txs.nonEmpty)
      require(persistentProver.digest.sameElements(rootHash))
      require(storage.version.get.sameElements(rootHash))
      require(store.lastVersionID.get.data.sameElements(rootHash))

      //todo: make a special config flag, "paranoid mode", and use it for checks like one commented below
      //persistentProver.checkTree(true)

      val mods = boxChanges(txs).operations.map(ADProofs.changeToMod)
      //todo .get
      mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
        t.flatMap(_ => {
          val opRes = persistentProver.performOneOperation(m)
          if (opRes.isFailure) log.warn(s"modification: $m, failure $opRes")
          opRes
        })
      }.get

      val proof = persistentProver.generateProofAndUpdateStorage()

      val digest = persistentProver.digest

      proof -> digest
    } match {
      case Success(res) => rollback().map(_ => res)
      case Failure(e) => rollback().flatMap(_ => Failure(e))
    }
  }

  override lazy val rootHash: ADDigest = persistentProver.digest

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    val p = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    store.get(ByteArrayWrapper(version)) match {
      case Some(hash) =>
        p.rollback(ADDigest @@ hash.data).map { _ =>
          new UtxoState(version, store) {
            override protected lazy val persistentProver = p
          }
        }
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  //todo: don't use assert
  private[state] def checkTransactions(transactions: Seq[AnyoneCanSpendTransaction], expectedDigest: ADDigest) = Try {

    transactions.foreach(tx => tx.semanticValidity.get)

    val mods = boxChanges(transactions).operations.map(ADProofs.changeToMod)
    mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
      t.flatMap(_ => {
        persistentProver.performOneOperation(m)
      })
    }.get

    if(!expectedDigest.sameElements(persistentProver.digest)) {
      throw new Error(s"Digest after txs application is wrong. ${Algos.encode(expectedDigest)} expected, " +
        s"${Algos.encode(persistentProver.digest)} given")
    }
  }

  //todo: utxo snapshot could go here
  //todo: dont' use assert
  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] = mod match {
    case fb: ErgoFullBlock =>
      log.debug(s"Trying to apply full block with header ${fb.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)}")

      //todo: rollback if failure on the way
      checkTransactions(fb.blockTransactions.txs, fb.header.stateRoot) match {
        case Success(_) =>
          Try {
            val md = metadata(VersionTag @@ fb.id, fb.header.stateRoot)
            val proofBytes = persistentProver.generateProofAndUpdateStorage(md)
            val proofHash = ADProofs.proofDigest(proofBytes)
            log.info(s"Valid modifier ${fb.encodedId} with header ${fb.header.encodedId} applied to UtxoState with " +
              s"root hash ${Algos.encode(rootHash)}")
            assert(store.get(ByteArrayWrapper(fb.id)).exists(_.data sameElements fb.header.stateRoot))
            assert(store.rollbackVersions().exists(_.data sameElements fb.header.stateRoot))
            assert(fb.header.ADProofsRoot.sameElements(proofHash))
            assert(fb.header.stateRoot sameElements persistentProver.digest)
            new UtxoState(VersionTag @@ fb.id, store)
          }
        case Failure(e) =>
          log.warn(s"Error while applying full block with header ${fb.header.encodedId} to UTXOState with root" +
            s" ${Algos.encode(rootHash)}: ", e)
          Failure(e)
      }

    case h: Header =>
      Success(new UtxoState(VersionTag @@ h.id, this.store))

    case a: Any =>
      log.info(s"Unhandled modifier: $a")
      Failure(new Exception("unknown modifier"))
  }

  def boxById(id: ADKey): Option[AnyoneCanSpendNoncedBox] =
    persistentProver
      .unauthenticatedLookup(id)
      .map(AnyoneCanSpendNoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  def randomBox(): Option[AnyoneCanSpendNoncedBox] =
    persistentProver.avlProver.randomWalk().map(_._1).flatMap(boxById)


  override def rollbackVersions: Iterable[VersionTag] =
    persistentProver.storage.rollbackVersions.map(v => VersionTag @@ store.get(ByteArrayWrapper(Algos.hash(v))).get.data)

  override def validate(tx: AnyoneCanSpendTransaction): Try[Unit] = if (tx.boxIdsToOpen.forall { k =>
    persistentProver.unauthenticatedLookup(k).isDefined
  }) Success()
  else Failure(new Exception(s"Not all boxes of the transaction $tx are in the state"))
}

object UtxoState {
  private lazy val bestVersionKey = Algos.hash("best state version")

  private def metadata(modId: VersionTag, stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
  }

  def create(dir: File): UtxoState = {
    val store = new QuickStore(dir, keepVersions = 20) // todo: magic number, move to settings
    val dbVersion = store.get(ByteArrayWrapper(bestVersionKey)).map(VersionTag @@ _.data)
    new UtxoState(dbVersion.getOrElse(ErgoState.genesisStateVersion), store)
  }

  def fromBoxHolder(bh: BoxHolder, dir: File): UtxoState = {
    val p = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize))
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val store = new QuickStore(dir, keepVersions = 200) // todo: magic number, move to settings

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