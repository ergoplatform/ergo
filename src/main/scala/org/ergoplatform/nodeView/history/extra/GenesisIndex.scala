package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.nodeView.history.extra.ExtraIndexer._
import org.ergoplatform.nodeView.history.extra.IndexedContractTemplateSerializer.hashTreeTemplate
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.ChainSettings
import scorex.util.bytesToId

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.util.Try

/** The configured genesis UTXOs have box references, but no creating indexed transaction. */
object GenesisIndex {
  def boxes(settings: ChainSettings): Seq[IndexedErgoBox] =
    ErgoState.genesisBoxes(settings).zipWithIndex.map { case (box, index) =>
      new IndexedErgoBox(0, None, None, None, box, index.toLong + 1L)
    }

  def nextBoxIndex(settings: ChainSettings): Long = ErgoState.genesisBoxes(settings).size.toLong + 1L

  private def rows(settings: ChainSettings, rentEnabled: Boolean): Array[ExtraIndex] = {
    val addresses = mutable.LinkedHashMap.empty[scorex.util.ModifierId, IndexedErgoAddress]
    val templates = mutable.LinkedHashMap.empty[scorex.util.ModifierId, IndexedContractTemplate]
    val genesis = boxes(settings)
    genesis.foreach { box =>
      val address = hashErgoTree(box.box.ergoTree)
      addresses.getOrElseUpdate(address, IndexedErgoAddress(address).initBalance).addBox(box)
      val template = hashTreeTemplate(box.box.ergoTree)
      templates.getOrElseUpdate(template, IndexedContractTemplate(template)).addBox(box)
    }
    (genesis ++ genesis.map(box => NumericBoxIndex(box.globalIndex, box.id)) ++
      addresses.values ++ templates.values ++
      (if (rentEnabled) genesis.map(StorageRentBox(_)) else Seq.empty)).toArray[ExtraIndex]
  }

  /** Install schema, genesis rows and their checkpoint in the same database batch. */
  def initialize(storage: HistoryStorage, settings: ChainSettings, rentEnabled: Boolean): Try[Unit] = {
    val zero = ByteBuffer.allocate(4).putInt(0).array
    val entries = Array(
      SchemaVersionKey -> NewestVersionBytes,
      IndexedHeightKey -> zero,
      GlobalTxIndexKey -> ByteBuffer.allocate(8).putLong(0L).array,
      GlobalBoxIndexKey -> ByteBuffer.allocate(8).putLong(nextBoxIndex(settings)).array,
      RollbackToKey -> zero
    ) ++ (if (rentEnabled) Array(RentIndexEnabledKey -> RentIndexEnabledBytes) else Array.empty[(Array[Byte], Array[Byte])])
    storage.insertExtraTry(entries, rows(settings, rentEnabled))
  }

  /** Check configured identity even after genesis boxes have been spent. */
  def identitiesMatch(storage: HistoryStorage, settings: ChainSettings): Boolean =
    storage.modifierBytesById(bytesToId(NumericBoxIndex.indexToBytes(0L))).isEmpty && boxes(settings).forall { expected =>
      storage.getExtraIndex(NumericBoxIndex(expected.globalIndex, expected.id).id).contains(
        NumericBoxIndex(expected.globalIndex, expected.id)) &&
        storage.getExtraIndex(expected.id).exists {
          case box: IndexedErgoBox => box.globalIndex == expected.globalIndex && box.inclusionHeight == 0 &&
            box.box.bytes.sameElements(expected.box.bytes)
          case _ => false
        }
    }

  /** At height zero every bootstrap row must still have its initial bytes. */
  def initialRowsMatch(storage: HistoryStorage, settings: ChainSettings, rentEnabled: Boolean): Boolean =
    rows(settings, rentEnabled).forall { row =>
      storage.modifierBytesById(row.id).exists(_.sameElements(ExtraIndexSerializer.toBytes(row)))
    }
}
