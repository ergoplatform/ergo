package org.ergoplatform.nodeView.wallet

import akka.util.ByteString
import org.ergoplatform.nodeView.wallet.scanning.{Scan, ScanWalletInteraction, ScanningPredicateSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import scorex.crypto.hash.Blake2b256
import scorex.util.serialization.{Reader, VLQByteStringWriter, Writer}

import java.nio.charset.StandardCharsets
import scala.util.Try

/** Immutable identity of the inputs which affect UTXO snapshot wallet scanning. */
final case class UtxoSnapshotScanDefinition(semanticsVersion: Byte, digest: ByteString) {
  require(
    semanticsVersion == UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
    s"Unsupported UTXO snapshot scan semantics version $semanticsVersion")
  require(digest != null, "UTXO snapshot scan definition digest must not be null")
  require(
    digest.length == UtxoSnapshotScanDefinition.DigestLength,
    s"UTXO snapshot scan definition digest must be ${UtxoSnapshotScanDefinition.DigestLength} bytes")
  require(
    digest.exists(_ != 0.toByte),
    "UTXO snapshot scan definition digest must not be all zero")
}

object UtxoSnapshotScanDefinition {
  val WalletScanSemanticsVersion: Byte = 1
  val DigestLength: Int = 32

  private val DomainSeparator: ByteString = ByteString(
    "ergo.wallet.utxo-snapshot-scan-definition".getBytes(StandardCharsets.US_ASCII))

  private val UnsignedByteStringOrdering: Ordering[ByteString] = new Ordering[ByteString] {
    override def compare(left: ByteString, right: ByteString): Int = {
      val commonLength = math.min(left.length, right.length)
      var index = 0
      while (index < commonLength) {
        val comparison = java.lang.Integer.compare(
          java.lang.Byte.toUnsignedInt(left(index)),
          java.lang.Byte.toUnsignedInt(right(index)))
        if (comparison != 0) {
          return comparison
        }
        index += 1
      }
      java.lang.Integer.compare(left.length, right.length)
    }
  }

  /** Calculate the definition used by the live wallet scan path. */
  def calculate(walletVars: WalletVars,
                dustLimit: Option[Long]): Try[UtxoSnapshotScanDefinition] = {
    Try {
      require(walletVars != null, "Wallet variables must not be null")
      (
        walletVars.trackedBytes,
        walletVars.miningScriptsBytes,
        walletVars.settings.miningRewardDelay,
        walletVars.externalScans
      )
    }.flatMap { case (tracked, mining, rewardDelay, scans) =>
      calculate(tracked, mining, rewardDelay, scans, dustLimit)
    }
  }

  /**
    * Calculate from the exact result-affecting inputs. This overload is package-visible so direct
    * tests can isolate every field without constructing prover or cache state.
    */
  private[wallet] def calculate(trackedPropositionBytes: Seq[Array[Byte]],
                                miningPropositionBytes: Seq[Array[Byte]],
                                miningRewardDelay: Int,
                                externalScans: Seq[Scan],
                                dustLimit: Option[Long]): Try[UtxoSnapshotScanDefinition] = {
    canonicalPayload(
      trackedPropositionBytes,
      miningPropositionBytes,
      miningRewardDelay,
      externalScans,
      dustLimit).map { payload =>
      val digest = Blake2b256.hash((DomainSeparator ++ payload).toArray)
      UtxoSnapshotScanDefinition(WalletScanSemanticsVersion, ByteString(digest))
    }
  }

  /** Exact canonical payload hashed after the fixed ASCII domain separator. */
  private[wallet] def canonicalPayload(trackedPropositionBytes: Seq[Array[Byte]],
                                       miningPropositionBytes: Seq[Array[Byte]],
                                       miningRewardDelay: Int,
                                       externalScans: Seq[Scan],
                                       dustLimit: Option[Long]): Try[ByteString] = Try {
    val tracked = canonicalScripts(trackedPropositionBytes, "tracked")
    val mining = canonicalScripts(miningPropositionBytes, "mining")
    require(externalScans != null, "External scans must not be null")
    require(externalScans.forall(_ != null), "External scans must not contain null")

    val scanIds = externalScans.map(_.scanId)
    require(scanIds.distinct.length == scanIds.length, "Duplicate external scan ID")
    val scans = externalScans.sortBy(_.scanId.toShort)

    val writer = new VLQByteStringWriter()
    writer.put(WalletScanSemanticsVersion)
    putBlobs(writer, tracked)
    putBlobs(writer, mining)
    writer.put(if (miningRewardDelay > 0) 1.toByte else 0.toByte)
    writer.putUInt(scans.length)
    scans.foreach { scan =>
      writer.putShort(scan.scanId)
      writer.put(ScanWalletInteraction.toByte(scan.walletInteraction))
      val predicateBytes = ScanningPredicateSerializer.toBytes(scan.trackingRule)
      writer.putUInt(predicateBytes.length)
      writer.putBytes(predicateBytes)
    }
    dustLimit match {
      case None =>
        writer.put(0.toByte)
      case Some(value) =>
        writer.put(1.toByte)
        writer.putLong(value)
    }
    writer.result()
  }

  private def canonicalScripts(scripts: Seq[Array[Byte]], label: String): Seq[ByteString] = {
    require(scripts != null, s"$label proposition bytes must not be null")
    scripts.map { bytes =>
      require(bytes != null, s"$label proposition bytes must not contain null")
      ByteString(bytes)
    }.distinct.sorted(UnsignedByteStringOrdering)
  }

  private def putBlobs(writer: Writer, blobs: Seq[ByteString]): Unit = {
    writer.putUInt(blobs.length)
    blobs.foreach { bytes =>
      writer.putUInt(bytes.length)
      writer.putBytes(bytes.toArray)
    }
  }
}

/** Strict embedded codec shared by status and origin serializers. */
object UtxoSnapshotScanDefinitionSerializer extends ErgoSerializer[UtxoSnapshotScanDefinition] {
  override def serialize(definition: UtxoSnapshotScanDefinition, writer: Writer): Unit = {
    writer.put(definition.semanticsVersion)
    writer.putBytes(definition.digest.toArray)
  }

  override def parse(reader: Reader): UtxoSnapshotScanDefinition = {
    UtxoSnapshotScanDefinition(
      semanticsVersion = reader.getByte(),
      digest = ByteString(reader.getBytes(UtxoSnapshotScanDefinition.DigestLength)))
  }
}
