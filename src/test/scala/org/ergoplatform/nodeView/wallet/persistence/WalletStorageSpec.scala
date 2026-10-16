package org.ergoplatform.nodeView.wallet.persistence

import akka.util.ByteString
import com.google.common.primitives.Ints
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.{UtxoSnapshotScanDefinition, UtxoSnapshotScanDefinitionSerializer}
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage.SecretPathsKey
import org.ergoplatform.nodeView.wallet.scanning.{ScanRequest, ScanWalletInteraction}
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, DerivationPathSerializer}
import org.ergoplatform.utils.generators.CoreObjectGenerators._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.db.LDBKVStore
import scorex.util.{ByteArrayBuilder, idToBytes}
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicInteger

class WalletStorageSpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with DBSpec {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import org.ergoplatform.wallet.utils.WalletGenerators._

  private val ScanDefinitionA = UtxoSnapshotScanDefinition(
    UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
    ByteString(Array.fill(UtxoSnapshotScanDefinition.DigestLength)(0x11.toByte)))
  private val ScanDefinitionB = UtxoSnapshotScanDefinition(
    UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
    ByteString(Array.fill(UtxoSnapshotScanDefinition.DigestLength)(0x22.toByte)))

  private def snapshotStatus(snapshotHeight: Int,
                             snapshotBlockId: scorex.util.ModifierId,
                             manifestDepth: Int,
                             nextSubtreeIndex: Int,
                             totalSubtrees: Int,
                             completed: Boolean): UtxoSnapshotScanStatus =
    new UtxoSnapshotScanStatus(
      snapshotHeight, snapshotBlockId, manifestDepth, nextSubtreeIndex,
      totalSubtrees, completed, ScanDefinitionA)

  private def snapshotStatus(snapshotHeight: Int,
                             snapshotBlockId: scorex.util.ModifierId,
                             manifestDepth: Int,
                             nextSubtreeIndex: Int,
                             totalSubtrees: Int,
                             completed: Boolean,
                             scanDefinition: UtxoSnapshotScanDefinition): UtxoSnapshotScanStatus =
    new UtxoSnapshotScanStatus(
      snapshotHeight, snapshotBlockId, manifestDepth, nextSubtreeIndex,
      totalSubtrees, completed, scanDefinition)

  private def snapshotOrigin(snapshotHeight: Int,
                             snapshotBlockId: scorex.util.ModifierId): UtxoSnapshotWalletOrigin =
    new UtxoSnapshotWalletOrigin(snapshotHeight, snapshotBlockId, ScanDefinitionA)

  private def snapshotOrigin(snapshotHeight: Int,
                             snapshotBlockId: scorex.util.ModifierId,
                             scanDefinition: UtxoSnapshotScanDefinition): UtxoSnapshotWalletOrigin =
    new UtxoSnapshotWalletOrigin(snapshotHeight, snapshotBlockId, scanDefinition)

  it should "add and read derivation paths" in {
    def addPath(store: LDBKVStore, storedPaths: Seq[DerivationPath], derivationPath: DerivationPath): Unit = {
      val updatedPaths = (storedPaths :+ derivationPath).toSet
      val toInsert = Ints.toByteArray(updatedPaths.size) ++ updatedPaths
        .foldLeft(Array.empty[Byte]) { case (acc, path) =>
          val bytes = DerivationPathSerializer.toBytes(path)
          acc ++ Ints.toByteArray(bytes.length) ++ bytes
        }
      store.insert(SecretPathsKey, toInsert).get
    }

    forAll(Gen.nonEmptyListOf(derivationPathGen)) { paths =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        paths.foreach(path => addPath(store, storage.readPaths(), path))
        storage.readPaths() should contain theSameElementsAs paths.toSet
      }
    }
  }

  it should "add and read public keys" in {
    forAll(extendedPubKeyListGen) { pubKeys =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        pubKeys.foreach(storage.addPublicKey(_).get)
        val keysRead = storage.readAllKeys()
        keysRead.length shouldBe pubKeys.length
        keysRead should contain theSameElementsAs pubKeys.toSet
      }
    }
  }

  it should "add, remove and read scans" in {
    forAll(Gen.nonEmptyListOf(externalScanReqGen)) { externalScanReqs =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        externalScanReqs.foreach(req => storage.addScan(req))
        val storageApps = storage.allScans
        val storageRequests = storageApps.map { app =>
          ScanRequest(app.scanName, app.trackingRule, Some(ScanWalletInteraction.Off), Some(true))
        }
        storageRequests.foreach(r => externalScanReqs.contains(r) shouldBe true)
        storageApps.map(_.scanId).foreach(storage.removeScan(_).get)
        storage.allScans.length shouldBe 0
      }
    }
  }

  it should "always increase ids" in {
    forAll(externalScanReqGen) { externalScanReq =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val scan = storage.addScan(externalScanReq).get

        storage.lastUsedScanId shouldBe scan.scanId

        storage.removeScan(scan.scanId).get
        storage.lastUsedScanId shouldBe scan.scanId

        val scan2 = storage.addScan(externalScanReq).get
        storage.lastUsedScanId shouldBe scan2.scanId
        storage.lastUsedScanId shouldBe (scan.scanId +1)
      }
    }
  }

  it should "store, update and remove UTXO snapshot scan status" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val status = snapshotStatus(
          snapshotHeight = 100,
          snapshotBlockId = snapshotBlockId,
          manifestDepth = 6,
          nextSubtreeIndex = 3,
          totalSubtrees = 64,
          completed = false
        )

        storage.readUtxoSnapshotScanStatus() shouldBe None
        storage.writeUtxoSnapshotScanStatus(status).get
        storage.readUtxoSnapshotScanStatus() shouldBe Some(status)

        val completedStatus = status.copy(nextSubtreeIndex = status.totalSubtrees, completed = true)
        storage.writeUtxoSnapshotScanStatus(completedStatus).get
        storage.readUtxoSnapshotScanStatus() shouldBe Some(completedStatus)

        storage.removeUtxoSnapshotScanStatus().get
        storage.readUtxoSnapshotScanStatus() shouldBe None
      }
    }
  }

  it should "round-trip the exact versioned UTXO snapshot scan status bytes with its definition" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val status = snapshotStatus(
          snapshotHeight = 100,
          snapshotBlockId = snapshotBlockId,
          manifestDepth = 6,
          nextSubtreeIndex = 3,
          totalSubtrees = 64,
          completed = false,
          scanDefinition = ScanDefinitionA)
        val payload = new VLQByteBufferWriter(new ByteArrayBuilder())
        payload.putUInt(status.snapshotHeight)
        payload.putBytes(idToBytes(status.snapshotBlockId))
        payload.putUInt(status.manifestDepth)
        payload.putUInt(status.nextSubtreeIndex)
        payload.putUInt(status.totalSubtrees)
        payload.putBoolean(status.completed)
        UtxoSnapshotScanDefinitionSerializer.serialize(status.scanDefinition, payload)
        val expectedBytes = Array[Byte](
          0x80.toByte, 0x00.toByte, 0x55.toByte, 0x57.toByte,
          0x53.toByte, 0x53.toByte, 0x01.toByte) ++ payload.result().toBytes

        UtxoSnapshotScanStatusSerializer.toBytes(status) should
          contain theSameElementsInOrderAs expectedBytes
        store.insert(WalletStorage.UtxoSnapshotScanStatusKey, expectedBytes).get
        storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
      }
    }
  }

  it should "reject every legacy or malformed UTXO snapshot scan status without inference" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val valid = snapshotStatus(
          100, snapshotBlockId, 6, 1, 2, completed = false,
          scanDefinition = ScanDefinitionA)
        val validBytes = UtxoSnapshotScanStatusSerializer.toBytes(valid)

        validBytes.indices.foreach { boundary =>
          store.insert(WalletStorage.UtxoSnapshotScanStatusKey, validBytes.take(boundary)).get
          withClue(s"truncation boundary $boundary") {
            storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true
          }
        }

        validBytes.take(7).indices.foreach { magicIndex =>
          val wrongMagic = validBytes.clone()
          wrongMagic(magicIndex) = (wrongMagic(magicIndex) ^ 0x01).toByte
          store.insert(WalletStorage.UtxoSnapshotScanStatusKey, wrongMagic).get
          withClue(s"status magic byte $magicIndex") {
            storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true
          }
        }

        val wrongSemantics = validBytes.clone()
        wrongSemantics(wrongSemantics.length - UtxoSnapshotScanDefinition.DigestLength - 1) =
          (UtxoSnapshotScanDefinition.WalletScanSemanticsVersion + 1).toByte
        store.insert(WalletStorage.UtxoSnapshotScanStatusKey, wrongSemantics).get
        storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true

        store.insert(WalletStorage.UtxoSnapshotScanStatusKey, validBytes :+ 0.toByte).get
        storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true

        Seq(0, 1, 127, 128, 16383, 16384, Int.MaxValue).foreach { height =>
          Seq((0, false), (1, false), (2, true)).foreach { case (nextSubtreeIndex, completed) =>
            val legacy = new VLQByteBufferWriter(new ByteArrayBuilder())
            legacy.putUInt(height)
            legacy.putBytes(idToBytes(snapshotBlockId))
            legacy.putUInt(6)
            legacy.putUInt(nextSubtreeIndex)
            legacy.putUInt(2)
            legacy.putBoolean(completed)
            val legacyBytes = legacy.result().toBytes
            store.insert(WalletStorage.UtxoSnapshotScanStatusKey, legacyBytes).get
            withClue(s"legacy height=$height cursor=$nextSubtreeIndex completed=$completed") {
              storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true
              java.util.Arrays.equals(
                store.get(WalletStorage.UtxoSnapshotScanStatusKey).get,
                legacyBytes) shouldBe true
            }
          }
        }
      }
    }
  }

  it should "fail closed when UTXO snapshot scan status bytes are corrupt" in {
    withStore { store =>
      val storage = new WalletStorage(store, settings)
      store.insert(WalletStorage.UtxoSnapshotScanStatusKey, Array[Byte](1, 2, 3)).get

      storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true
    }
  }

  it should "reject non-canonical and internally inconsistent UTXO snapshot scan status" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val valid = snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)

        val trailing = UtxoSnapshotScanStatusSerializer.toBytes(valid) :+ 0.toByte
        store.insert(WalletStorage.UtxoSnapshotScanStatusKey, trailing).get
        storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true

        val inconsistent = valid.copy(nextSubtreeIndex = 2, completed = false)
        store.insert(
          WalletStorage.UtxoSnapshotScanStatusKey,
          UtxoSnapshotScanStatusSerializer.toBytes(inconsistent)).get
        storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true

        val completedStatus = valid.copy(nextSubtreeIndex = valid.totalSubtrees, completed = true)
        val nonCanonicalBoolean = UtxoSnapshotScanStatusSerializer.toBytes(completedStatus)
        val definitionLength =
          UtxoSnapshotScanDefinitionSerializer.toBytes(completedStatus.scanDefinition).length
        nonCanonicalBoolean(nonCanonicalBoolean.length - definitionLength - 1) = 2.toByte
        val nonCanonicalReader = new VLQByteBufferReader(ByteBuffer.wrap(nonCanonicalBoolean))
        UtxoSnapshotScanStatusSerializer.parse(nonCanonicalReader) shouldBe completedStatus
        nonCanonicalReader.remaining shouldBe 0
        storage.writeUtxoSnapshotScanStatus(completedStatus).isSuccess shouldBe true
        store.insert(WalletStorage.UtxoSnapshotScanStatusKey, nonCanonicalBoolean).get
        storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true

        storage.writeUtxoSnapshotScanStatus(valid.copy(totalSubtrees = 0)).isFailure shouldBe true
      }
    }
  }

  it should "round-trip the exact versioned UTXO snapshot wallet origin bytes" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val origin = snapshotOrigin(100, snapshotBlockId)
        val expectedWriter = new VLQByteBufferWriter(new ByteArrayBuilder())
        expectedWriter.put(2: Byte)
        expectedWriter.putUInt(origin.snapshotHeight)
        expectedWriter.putBytes(idToBytes(origin.snapshotBlockId))
        UtxoSnapshotScanDefinitionSerializer.serialize(origin.scanDefinition, expectedWriter)
        val expectedBytes = expectedWriter.result().toBytes

        UtxoSnapshotWalletOriginSerializer.toBytes(origin) should
          contain theSameElementsInOrderAs expectedBytes
        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, expectedBytes).get
        storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(origin)
      }
    }
  }

  it should "reject unknown, trailing, noncanonical, malformed, and overflow UTXO snapshot wallet origin bytes" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val origin = snapshotOrigin(100, snapshotBlockId)
        val validBytes = UtxoSnapshotWalletOriginSerializer.toBytes(origin)

        val unknownVersion = validBytes.clone()
        unknownVersion(0) = 3: Byte
        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, unknownVersion).get
        storage.readUtxoSnapshotWalletOriginTry().isFailure shouldBe true

        validBytes.indices.foreach { boundary =>
          store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, validBytes.take(boundary)).get
          withClue(s"origin truncation boundary $boundary") {
            storage.readUtxoSnapshotWalletOriginTry().isFailure shouldBe true
          }
        }

        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, validBytes :+ 0.toByte).get
        storage.readUtxoSnapshotWalletOriginTry().isFailure shouldBe true

        val nonCanonicalHeight = Array[Byte](2, 0xe4.toByte, 0x00.toByte) ++
          idToBytes(snapshotBlockId) ++ UtxoSnapshotScanDefinitionSerializer.toBytes(ScanDefinitionA)
        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, nonCanonicalHeight).get
        storage.readUtxoSnapshotWalletOriginTry().isFailure shouldBe true

        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, Array[Byte](2, 0x80.toByte)).get
        storage.readUtxoSnapshotWalletOriginTry().isFailure shouldBe true

        val wrongSemantics = validBytes.clone()
        wrongSemantics(wrongSemantics.length - UtxoSnapshotScanDefinition.DigestLength - 1) =
          (UtxoSnapshotScanDefinition.WalletScanSemanticsVersion + 1).toByte
        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, wrongSemantics).get
        storage.readUtxoSnapshotWalletOriginTry().isFailure shouldBe true

        val legacyV1 = new VLQByteBufferWriter(new ByteArrayBuilder())
        legacyV1.put(1: Byte)
        legacyV1.putUInt(origin.snapshotHeight)
        legacyV1.putBytes(idToBytes(origin.snapshotBlockId))
        val legacyBytes = legacyV1.result().toBytes
        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, legacyBytes).get
        storage.readUtxoSnapshotWalletOriginTry().isFailure shouldBe true
        java.util.Arrays.equals(
          store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get,
          legacyBytes) shouldBe true

        val overflowWriter = new VLQByteBufferWriter(new ByteArrayBuilder())
        overflowWriter.put(2: Byte)
        overflowWriter.putUInt(Int.MaxValue.toLong + 1L)
        overflowWriter.putBytes(idToBytes(snapshotBlockId))
        UtxoSnapshotScanDefinitionSerializer.serialize(ScanDefinitionA, overflowWriter)
        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, overflowWriter.result().toBytes).get
        storage.readUtxoSnapshotWalletOriginTry().isFailure shouldBe true
      }
    }
  }

  it should "complete UTXO snapshot status and absent origin in one atomic batch" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withDb { db =>
        val updateCalls = new AtomicInteger(0)
        val store = new LDBKVStore(db) {
          override def update(
            toInsertKeys: Array[Array[Byte]],
            toInsertValues: Array[Array[Byte]],
            toRemove: Array[Array[Byte]]): scala.util.Try[Unit] = {
            updateCalls.incrementAndGet()
            super.update(toInsertKeys, toInsertValues, toRemove)
          }
        }
        val storage = new WalletStorage(store, settings)
        val status = snapshotStatus(100, snapshotBlockId, 6, 64, 64, completed = true)
        val origin = snapshotOrigin(status.snapshotHeight, status.snapshotBlockId)

        storage.completeUtxoSnapshotScan(status).get

        updateCalls.get() shouldBe 1
        storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
        storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(origin)
      }
    }
  }

  it should "accept identical completed UTXO snapshot origin replay" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val status = snapshotStatus(100, snapshotBlockId, 6, 64, 64, completed = true)

        storage.completeUtxoSnapshotScan(status).get
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()
        val originBytes = store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get.clone()

        storage.completeUtxoSnapshotScan(status).get
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get, originBytes) shouldBe true
      }

    }
  }

  it should "reject height-only and block-id-only completed UTXO snapshot origin conflicts without mutation" in {
    forAll(modifierIdGen, modifierIdGen) { (snapshotBlockId, differentBlockId) =>
      whenever(snapshotBlockId != differentBlockId) {
        Seq(
          snapshotOrigin(101, snapshotBlockId),
          snapshotOrigin(100, differentBlockId)
        ).foreach { existingOrigin =>
          withStore { store =>
            val storage = new WalletStorage(store, settings)
            val oldStatus = snapshotStatus(
              existingOrigin.snapshotHeight,
              existingOrigin.snapshotBlockId,
              6,
              1,
              2,
              completed = false)
            val completed = snapshotStatus(100, snapshotBlockId, 6, 2, 2, completed = true)
            storage.writeUtxoSnapshotScanStatus(oldStatus).get
            store.insert(
              WalletStorage.UtxoSnapshotWalletOriginKey,
              UtxoSnapshotWalletOriginSerializer.toBytes(existingOrigin)).get
            val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()
            val originBytes = store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get.clone()

            storage.completeUtxoSnapshotScan(completed).isFailure shouldBe true

            java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
            java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get, originBytes) shouldBe true
          }
        }
      }
    }
  }

  it should "reject a definition-only completed UTXO snapshot origin conflict without mutation" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val completedA = snapshotStatus(
          100, snapshotBlockId, 6, 2, 2, completed = true,
          scanDefinition = ScanDefinitionA)
        val completedB = completedA.copy(scanDefinition = ScanDefinitionB)
        storage.completeUtxoSnapshotScan(completedB).get
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()
        val originBytes = store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get.clone()

        storage.completeUtxoSnapshotScan(completedA).isFailure shouldBe true

        java.util.Arrays.equals(
          store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
        java.util.Arrays.equals(
          store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get, originBytes) shouldBe true
      }
    }
  }

  it should "reject incomplete or inconsistent status without changing UTXO snapshot origin records" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)
        val origin = snapshotOrigin(oldStatus.snapshotHeight, oldStatus.snapshotBlockId)
        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        store.insert(
          WalletStorage.UtxoSnapshotWalletOriginKey,
          UtxoSnapshotWalletOriginSerializer.toBytes(origin)).get
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()
        val originBytes = store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get.clone()

        storage.completeUtxoSnapshotScan(oldStatus).isFailure shouldBe true
        storage.completeUtxoSnapshotScan(oldStatus.copy(nextSubtreeIndex = 2)).isFailure shouldBe true

        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get, originBytes) shouldBe true
      }
    }
  }

  it should "preserve exact status and origin bytes when the completion batch fails" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withDb { db =>
        val updateCalls = new AtomicInteger(0)
        val store = new LDBKVStore(db) {
          override def update(
            toInsertKeys: Array[Array[Byte]],
            toInsertValues: Array[Array[Byte]],
            toRemove: Array[Array[Byte]]): scala.util.Try[Unit] = {
            updateCalls.incrementAndGet()
            scala.util.Failure(new IllegalStateException("injected completion batch failure"))
          }
        }
        val storage = new WalletStorage(store, settings)
        val oldStatus: UtxoSnapshotScanStatus =
          snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)
        val completed = oldStatus.copy(nextSubtreeIndex = 2, completed = true)
        val origin = snapshotOrigin(oldStatus.snapshotHeight, oldStatus.snapshotBlockId)
        store.insert(WalletStorage.UtxoSnapshotScanStatusKey, UtxoSnapshotScanStatusSerializer.toBytes(oldStatus)).get
        store.insert(
          WalletStorage.UtxoSnapshotWalletOriginKey,
          UtxoSnapshotWalletOriginSerializer.toBytes(origin)).get
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()
        val originBytes = store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get.clone()

        storage.completeUtxoSnapshotScan(completed).isFailure shouldBe true

        updateCalls.get() shouldBe 1
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get, originBytes) shouldBe true
      }

      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val oldStatus: UtxoSnapshotScanStatus =
          snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)
        val completed = oldStatus.copy(nextSubtreeIndex = 2, completed = true)
        val corruptOriginBytes = Array[Byte](2, 0x80.toByte)
        store.insert(
          WalletStorage.UtxoSnapshotScanStatusKey,
          UtxoSnapshotScanStatusSerializer.toBytes(oldStatus)).get
        store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, corruptOriginBytes).get
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()
        val originBytes = store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get.clone()

        storage.completeUtxoSnapshotScan(completed).isFailure shouldBe true

        java.util.Arrays.equals(
          store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
        java.util.Arrays.equals(
          store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get, originBytes) shouldBe true
      }
    }
  }

  it should "round-trip UTXO snapshot scan invalidation without rewriting scan status" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val invalidation = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val status = snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)

        storage.writeUtxoSnapshotScanStatus(status).get
        storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None

        storage.writeUtxoSnapshotScanInvalidation(invalidation).get

        storage.readUtxoSnapshotScanInvalidationTry().get shouldBe Some(invalidation)
        storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
      }
    }
  }

  it should "accept the same UTXO snapshot invalidation idempotently without rewriting recovery records" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val invalidation = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val status = snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)

        storage.writeUtxoSnapshotScanStatus(status).get
        storage.writeUtxoSnapshotScanInvalidation(invalidation).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.writeUtxoSnapshotScanInvalidation(invalidation).isSuccess shouldBe true
        java.util.Arrays.equals(
          store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get,
          markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "not overwrite an existing UTXO snapshot invalidation when only the height differs" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val current = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val conflicting = current.copy(snapshotHeight = 101)
        val status = snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)

        storage.writeUtxoSnapshotScanStatus(status).get
        storage.writeUtxoSnapshotScanInvalidation(current).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.writeUtxoSnapshotScanInvalidation(conflicting).isFailure shouldBe true
        java.util.Arrays.equals(
          store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get,
          markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "not overwrite an existing UTXO snapshot invalidation when only the block id differs" in {
    forAll(modifierIdGen, modifierIdGen) { (snapshotBlockId, conflictingBlockId) =>
      whenever(snapshotBlockId != conflictingBlockId) {
        withStore { store =>
          val storage = new WalletStorage(store, settings)
          val current = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
          val conflicting = UtxoSnapshotScanInvalidation(100, conflictingBlockId)
          val status = snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)

          storage.writeUtxoSnapshotScanStatus(status).get
          storage.writeUtxoSnapshotScanInvalidation(current).get
          val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
          val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

          storage.writeUtxoSnapshotScanInvalidation(conflicting).isFailure shouldBe true
          java.util.Arrays.equals(
            store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get,
            markerBytes) shouldBe true
          java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
        }
      }
    }
  }

  it should "not replace corrupt UTXO snapshot invalidation bytes" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val invalidation = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val status = snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)
        val corruptMarkerBytes = Array[Byte](1, 2, 3)

        storage.writeUtxoSnapshotScanStatus(status).get
        store.insert(WalletStorage.UtxoSnapshotScanInvalidationKey, corruptMarkerBytes).get
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.writeUtxoSnapshotScanInvalidation(invalidation).isFailure shouldBe true
        java.util.Arrays.equals(
          store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get,
          corruptMarkerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "reject malformed, trailing, and negative-height UTXO snapshot scan invalidation" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val valid = UtxoSnapshotScanInvalidation(100, snapshotBlockId)

        store.insert(WalletStorage.UtxoSnapshotScanInvalidationKey, Array[Byte](1, 2, 3)).get
        storage.readUtxoSnapshotScanInvalidationTry().isFailure shouldBe true

        val trailing = UtxoSnapshotScanInvalidationSerializer.toBytes(valid) :+ 0.toByte
        store.insert(WalletStorage.UtxoSnapshotScanInvalidationKey, trailing).get
        storage.readUtxoSnapshotScanInvalidationTry().isFailure shouldBe true

        storage.writeUtxoSnapshotScanInvalidation(valid.copy(snapshotHeight = -1)).isFailure shouldBe true
      }
    }
  }

  it should "reject a non-minimal VLQ height in UTXO snapshot scan invalidation" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val nonMinimalHeight = Array(0xe4.toByte, 0x00.toByte) ++ scorex.util.idToBytes(snapshotBlockId)

        store.insert(WalletStorage.UtxoSnapshotScanInvalidationKey, nonMinimalHeight).get

        storage.readUtxoSnapshotScanInvalidationTry().isFailure shouldBe true
      }
    }
  }

  it should "not clear UTXO snapshot recovery when only the invalidation height differs" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val current = UtxoSnapshotScanInvalidation(101, snapshotBlockId)
        val status = snapshotStatus(101, snapshotBlockId, 6, 1, 2, completed = false)

        storage.writeUtxoSnapshotScanStatus(status).get
        storage.writeUtxoSnapshotScanInvalidation(current).get

        storage.clearUtxoSnapshotScanRecovery(expected).get shouldBe false
        storage.readUtxoSnapshotScanInvalidationTry().get shouldBe Some(current)
        storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
      }
    }
  }

  it should "not clear UTXO snapshot recovery when only the invalidation block id differs" in {
    forAll(modifierIdGen, modifierIdGen) { (snapshotBlockId, differentBlockId) =>
      whenever(snapshotBlockId != differentBlockId) {
        withStore { store =>
          val storage = new WalletStorage(store, settings)
          val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
          val current = UtxoSnapshotScanInvalidation(100, differentBlockId)
          val status = snapshotStatus(100, differentBlockId, 6, 1, 2, completed = false)

          storage.writeUtxoSnapshotScanStatus(status).get
          storage.writeUtxoSnapshotScanInvalidation(current).get

          storage.clearUtxoSnapshotScanRecovery(expected).get shouldBe false
          storage.readUtxoSnapshotScanInvalidationTry().get shouldBe Some(current)
          storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
        }
      }
    }
  }

  it should "clear both UTXO snapshot recovery records for the expected invalidation marker" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val invalidation = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val status = snapshotStatus(100, snapshotBlockId, 6, 1, 2, completed = false)

        storage.writeUtxoSnapshotScanStatus(status).get
        storage.writeUtxoSnapshotScanInvalidation(invalidation).get

        storage.clearUtxoSnapshotScanRecovery(invalidation).get shouldBe true
        storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
        storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      }
    }
  }

  it should "atomically replace arbitrary scan progress and remove the matching recovery fence" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val freshStatus = snapshotStatus(100, snapshotBlockId, 6, 0, 64, completed = false)
        val arbitraryOldStatusBytes = Array[Byte](1, 2, 3)

        store.insert(WalletStorage.UtxoSnapshotScanStatusKey, arbitraryOldStatusBytes).get
        storage.writeUtxoSnapshotScanInvalidation(expected).get

        storage.restartUtxoSnapshotScanRecovery(expected, freshStatus).get shouldBe true
        storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
        storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(freshStatus)
        storage.readUtxoSnapshotWalletOriginTry().get shouldBe None
      }
    }
  }

  it should "preserve the same UTXO snapshot origin during recovery restart" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val freshStatus = snapshotStatus(100, snapshotBlockId, 6, 0, 64, completed = false)
        val origin = snapshotOrigin(expected.snapshotHeight, expected.snapshotBlockId)
        storage.writeUtxoSnapshotScanInvalidation(expected).get
        store.insert(
          WalletStorage.UtxoSnapshotWalletOriginKey,
          UtxoSnapshotWalletOriginSerializer.toBytes(origin)).get
        val originBytes = store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, freshStatus).get shouldBe true

        storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
        storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(freshStatus)
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get, originBytes) shouldBe true
      }
    }
  }

  it should "reject conflicting or corrupt UTXO snapshot origin during recovery restart without mutation" in {
    forAll(modifierIdGen, modifierIdGen) { (snapshotBlockId, differentBlockId) =>
      whenever(snapshotBlockId != differentBlockId) {
        val originCases = Seq(
          UtxoSnapshotWalletOriginSerializer.toBytes(snapshotOrigin(101, snapshotBlockId)),
          UtxoSnapshotWalletOriginSerializer.toBytes(snapshotOrigin(100, differentBlockId)),
          UtxoSnapshotWalletOriginSerializer.toBytes(
            snapshotOrigin(100, snapshotBlockId, ScanDefinitionB)),
          Array[Byte](1, 2, 3)
        )
        originCases.foreach { originBytesToStore =>
          withStore { store =>
            val storage = new WalletStorage(store, settings)
            val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
            val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
            val freshStatus = oldStatus.copy(nextSubtreeIndex = 0)
            storage.writeUtxoSnapshotScanStatus(oldStatus).get
            storage.writeUtxoSnapshotScanInvalidation(expected).get
            store.insert(WalletStorage.UtxoSnapshotWalletOriginKey, originBytesToStore).get
            val fenceBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
            val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()
            val originBytes = store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get.clone()

            storage.restartUtxoSnapshotScanRecovery(expected, freshStatus).isFailure shouldBe true

            java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, fenceBytes) shouldBe true
            java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
            java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotWalletOriginKey).get, originBytes) shouldBe true
          }
        }
      }
    }
  }

  it should "leave scan progress unchanged when the recovery fence is absent" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
        val freshStatus = oldStatus.copy(nextSubtreeIndex = 0)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, freshStatus).get shouldBe false
        storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "leave both recovery records unchanged when only the fence height differs" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val current = expected.copy(snapshotHeight = 101)
        val oldStatus = snapshotStatus(101, snapshotBlockId, 6, 3, 64, completed = false)
        val freshStatus = snapshotStatus(100, snapshotBlockId, 6, 0, 64, completed = false)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(current).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, freshStatus).get shouldBe false
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "leave both recovery records unchanged when only the fence block id differs" in {
    forAll(modifierIdGen, modifierIdGen) { (snapshotBlockId, differentBlockId) =>
      whenever(snapshotBlockId != differentBlockId) {
        withStore { store =>
          val storage = new WalletStorage(store, settings)
          val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
          val current = UtxoSnapshotScanInvalidation(100, differentBlockId)
          val oldStatus = snapshotStatus(100, differentBlockId, 6, 3, 64, completed = false)
          val freshStatus = snapshotStatus(100, snapshotBlockId, 6, 0, 64, completed = false)

          storage.writeUtxoSnapshotScanStatus(oldStatus).get
          storage.writeUtxoSnapshotScanInvalidation(current).get
          val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
          val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

          storage.restartUtxoSnapshotScanRecovery(expected, freshStatus).get shouldBe false
          java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
          java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
        }
      }
    }
  }

  it should "fail a recovery restart without changing corrupt fence bytes or scan progress" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
        val freshStatus = oldStatus.copy(nextSubtreeIndex = 0)
        val corruptMarkerBytes = Array[Byte](1, 2, 3)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        store.insert(WalletStorage.UtxoSnapshotScanInvalidationKey, corruptMarkerBytes).get
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, freshStatus).isFailure shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, corruptMarkerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "reject a non-zero fresh recovery cursor without changing either recovery record" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
        val invalidFreshStatus = oldStatus.copy(nextSubtreeIndex = 1)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(expected).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, invalidFreshStatus).isFailure shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "reject completed fresh recovery progress without changing either recovery record" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
        val invalidFreshStatus = oldStatus.copy(nextSubtreeIndex = 64, completed = true)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(expected).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, invalidFreshStatus).isFailure shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "reject a negative fresh recovery manifest depth without changing either recovery record" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
        val invalidFreshStatus = oldStatus.copy(manifestDepth = -1, nextSubtreeIndex = 0)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(expected).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, invalidFreshStatus).isFailure shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "reject a non-positive fresh recovery subtree count without changing either recovery record" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
        val invalidFreshStatus = oldStatus.copy(nextSubtreeIndex = 0, totalSubtrees = 0)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(expected).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, invalidFreshStatus).isFailure shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "reject a fresh recovery height mismatch without changing either recovery record" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
        val invalidFreshStatus = oldStatus.copy(snapshotHeight = 101, nextSubtreeIndex = 0)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(expected).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, invalidFreshStatus).isFailure shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

  it should "reject a fresh recovery block id mismatch without changing either recovery record" in {
    forAll(modifierIdGen, modifierIdGen) { (snapshotBlockId, differentBlockId) =>
      whenever(snapshotBlockId != differentBlockId) {
        withStore { store =>
          val storage = new WalletStorage(store, settings)
          val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
          val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
          val invalidFreshStatus = oldStatus.copy(snapshotBlockId = differentBlockId, nextSubtreeIndex = 0)

          storage.writeUtxoSnapshotScanStatus(oldStatus).get
          storage.writeUtxoSnapshotScanInvalidation(expected).get
          val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
          val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

          storage.restartUtxoSnapshotScanRecovery(expected, invalidFreshStatus).isFailure shouldBe true
          java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
          java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
        }
      }
    }
  }

  it should "preserve both recovery records when the atomic restart batch fails" in {
    forAll(modifierIdGen) { snapshotBlockId =>
      withDb { db =>
        var updateCalls = 0
        val store = new LDBKVStore(db) {
          override def update(
            toInsertKeys: Array[Array[Byte]],
            toInsertValues: Array[Array[Byte]],
            toRemove: Array[Array[Byte]]): scala.util.Try[Unit] = {
            updateCalls += 1
            scala.util.Failure(new IllegalStateException("injected atomic restart failure"))
          }
        }
        val storage = new WalletStorage(store, settings)
        val expected = UtxoSnapshotScanInvalidation(100, snapshotBlockId)
        val oldStatus = snapshotStatus(100, snapshotBlockId, 6, 3, 64, completed = false)
        val freshStatus = oldStatus.copy(nextSubtreeIndex = 0)

        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(expected).get
        val markerBytes = store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get.clone()
        val statusBytes = store.get(WalletStorage.UtxoSnapshotScanStatusKey).get.clone()

        storage.restartUtxoSnapshotScanRecovery(expected, freshStatus).isFailure shouldBe true
        updateCalls shouldBe 1
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanInvalidationKey).get, markerBytes) shouldBe true
        java.util.Arrays.equals(store.get(WalletStorage.UtxoSnapshotScanStatusKey).get, statusBytes) shouldBe true
      }
    }
  }

}
