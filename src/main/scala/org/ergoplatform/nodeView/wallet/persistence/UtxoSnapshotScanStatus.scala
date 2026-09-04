package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import org.ergoplatform.nodeView.wallet.{UtxoSnapshotScanDefinition, UtxoSnapshotScanDefinitionSerializer}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

/**
  * Versioned wallet progress for scanning a bootstrapped UTXO snapshot,
  * bound to an immutable scan definition.
  */
final case class UtxoSnapshotScanStatus(snapshotHeight: Int,
                                        snapshotBlockId: ModifierId,
                                        manifestDepth: Int,
                                        nextSubtreeIndex: Int,
                                        totalSubtrees: Int,
                                        completed: Boolean,
                                        scanDefinition: UtxoSnapshotScanDefinition)

/**
  * Durable fence preventing recovery from treating a partially invalidated UTXO snapshot scan as resumable.
  */
final case class UtxoSnapshotScanInvalidation(snapshotHeight: Int, snapshotBlockId: ModifierId)

/** Durable provenance of a wallet projection completed from one immutable UTXO snapshot. */
final case class UtxoSnapshotWalletOrigin(snapshotHeight: Int,
                                          snapshotBlockId: ModifierId,
                                          scanDefinition: UtxoSnapshotScanDefinition)

object UtxoSnapshotScanStatusSerializer extends ErgoSerializer[UtxoSnapshotScanStatus] {
  private val FormatPrefix: Array[Byte] = Array(
    0x80.toByte, 0x00.toByte, 0x55.toByte, 0x57.toByte,
    0x53.toByte, 0x53.toByte, 0x01.toByte)

  override def serialize(obj: UtxoSnapshotScanStatus, w: Writer): Unit = {
    w.putBytes(FormatPrefix)
    w.putUInt(obj.snapshotHeight)
    w.putBytes(idToBytes(obj.snapshotBlockId))
    w.putUInt(obj.manifestDepth)
    w.putUInt(obj.nextSubtreeIndex)
    w.putUInt(obj.totalSubtrees)
    w.putBoolean(obj.completed)
    UtxoSnapshotScanDefinitionSerializer.serialize(obj.scanDefinition, w)
  }

  override def parse(r: Reader): UtxoSnapshotScanStatus = {
    val prefix = r.getBytes(FormatPrefix.length)
    require(java.util.Arrays.equals(prefix, FormatPrefix),
      "Unsupported UTXO snapshot scan status format")
    UtxoSnapshotScanStatus(
      snapshotHeight = r.getUIntExact(),
      snapshotBlockId = bytesToId(r.getBytes(Constants.ModifierIdSize)),
      manifestDepth = r.getUIntExact(),
      nextSubtreeIndex = r.getUIntExact(),
      totalSubtrees = r.getUIntExact(),
      completed = r.getByte() != 0,
      scanDefinition = UtxoSnapshotScanDefinitionSerializer.parse(r)
    )
  }
}

object UtxoSnapshotScanInvalidationSerializer extends ErgoSerializer[UtxoSnapshotScanInvalidation] {

  override def serialize(obj: UtxoSnapshotScanInvalidation, w: Writer): Unit = {
    w.putUInt(obj.snapshotHeight)
    w.putBytes(idToBytes(obj.snapshotBlockId))
  }

  override def parse(r: Reader): UtxoSnapshotScanInvalidation = {
    UtxoSnapshotScanInvalidation(
      snapshotHeight = r.getUIntExact(),
      snapshotBlockId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    )
  }
}

object UtxoSnapshotWalletOriginSerializer extends ErgoSerializer[UtxoSnapshotWalletOrigin] {
  private val FormatVersion: Byte = 2

  override def serialize(origin: UtxoSnapshotWalletOrigin, w: Writer): Unit = {
    w.put(FormatVersion)
    w.putUInt(origin.snapshotHeight)
    w.putBytes(idToBytes(origin.snapshotBlockId))
    UtxoSnapshotScanDefinitionSerializer.serialize(origin.scanDefinition, w)
  }

  override def parse(r: Reader): UtxoSnapshotWalletOrigin = {
    val version = r.getByte()
    require(version == FormatVersion, s"Unsupported UTXO snapshot wallet origin version $version")
    UtxoSnapshotWalletOrigin(
      snapshotHeight = r.getUIntExact(),
      snapshotBlockId = bytesToId(r.getBytes(Constants.ModifierIdSize)),
      scanDefinition = UtxoSnapshotScanDefinitionSerializer.parse(r)
    )
  }
}
