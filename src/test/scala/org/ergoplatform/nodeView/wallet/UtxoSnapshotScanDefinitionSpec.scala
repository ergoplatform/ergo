package org.ergoplatform.nodeView.wallet

import akka.util.ByteString
import org.ergoplatform.nodeView.wallet.scanning.{AndScanningPredicate, ContainsAssetPredicate, Scan, ScanWalletInteraction, ScanningPredicate}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.Constants.ScanId
import scorex.util.encode.Base16
import sigmastate.eval.Extensions.ArrayByteOps

class UtxoSnapshotScanDefinitionSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val BaseScanId: ScanId = ScanId @@ 50.toShort
  private val BasePredicate = ContainsAssetPredicate(Array.tabulate(32)(_.toByte).toTokenId)
  private val OtherPredicate = ContainsAssetPredicate(Array.fill(32)(0x55.toByte).toTokenId)
  private val BaseScan = Scan(BaseScanId, "base", BasePredicate, ScanWalletInteraction.Off, removeOffchain = true)

  private def calculate(tracked: Seq[Array[Byte]] = Seq(Array[Byte](1)),
                        mining: Seq[Array[Byte]] = Seq(Array[Byte](2)),
                        rewardDelay: Int = 720,
                        scans: Seq[Scan],
                        dustLimit: Option[Long] = None) = {
    UtxoSnapshotScanDefinition.calculate(tracked, mining, rewardDelay, scans, dustLimit)
  }

  private def definition(tracked: Seq[Array[Byte]] = Seq(Array[Byte](1)),
                         mining: Seq[Array[Byte]] = Seq(Array[Byte](2)),
                         rewardDelay: Int = 720,
                         scans: Seq[Scan] = Seq(BaseScan),
                         dustLimit: Option[Long] = None): UtxoSnapshotScanDefinition = {
    calculate(tracked, mining, rewardDelay, scans, dustLimit).get
  }

  property("definition has immutable content equality and validates its embedded wire shape") {
    val source = Array.fill(32)(7.toByte)
    val first = UtxoSnapshotScanDefinition(UtxoSnapshotScanDefinition.WalletScanSemanticsVersion, ByteString(source))
    val second = UtxoSnapshotScanDefinition(
      UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
      ByteString(source.clone()))

    first shouldBe second
    first.hashCode() shouldBe second.hashCode()

    source(0) = 8.toByte
    first.digest.head shouldBe 7.toByte

    val wrongSemanticsFailure = intercept[IllegalArgumentException] {
      UtxoSnapshotScanDefinition(
        0.toByte,
        ByteString(Array.tabulate(32)(index => (index + 1).toByte)))
    }
    wrongSemanticsFailure.getMessage shouldBe
      "requirement failed: Unsupported UTXO snapshot scan semantics version 0"
    intercept[IllegalArgumentException] {
      UtxoSnapshotScanDefinition(UtxoSnapshotScanDefinition.WalletScanSemanticsVersion, null)
    }
    val emptyDigestFailure = intercept[IllegalArgumentException] {
      UtxoSnapshotScanDefinition(
        UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
        ByteString.empty)
    }
    emptyDigestFailure.getMessage shouldBe
      "requirement failed: UTXO snapshot scan definition digest must be 32 bytes"
    Seq(31, 33).foreach { length =>
      intercept[IllegalArgumentException] {
        UtxoSnapshotScanDefinition(
          UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
          ByteString(Array.tabulate(length)(index => if (index == 0) 1.toByte else 0.toByte)))
      }
    }
    val zeroDigestFailure = intercept[IllegalArgumentException] {
      UtxoSnapshotScanDefinition(
        UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
        ByteString(Array.fill(32)(0.toByte)))
    }
    zeroDigestFailure.getMessage shouldBe
      "requirement failed: UTXO snapshot scan definition digest must not be all zero"
    val zeroWireFailure = intercept[IllegalArgumentException] {
      UtxoSnapshotScanDefinitionSerializer.parseBytes(
        Array(UtxoSnapshotScanDefinition.WalletScanSemanticsVersion) ++ Array.fill(32)(0.toByte))
    }
    zeroWireFailure.getMessage shouldBe zeroDigestFailure.getMessage

    Base16.encode(UtxoSnapshotScanDefinitionSerializer.toBytes(first)) shouldBe
      ("01" + List.fill(32)("07").mkString)
    UtxoSnapshotScanDefinitionSerializer.parseBytes(
      UtxoSnapshotScanDefinitionSerializer.toBytes(first)) shouldBe first
  }

  property("source-array mutation cannot change a calculated definition") {
    val source = Array[Byte](1, 2, 3)
    val calculated = definition(tracked = Seq(source), mining = Seq.empty, scans = Seq.empty)
    val digestBefore = calculated.digest

    source(0) = 99.toByte

    calculated.digest shouldBe digestBefore
    calculated shouldBe definition(tracked = Seq(Array[Byte](1, 2, 3)), mining = Seq.empty, scans = Seq.empty)
  }

  property("script permutations and content duplicates are canonical within each category") {
    val trackedA = Seq(Array[Byte](0x80.toByte), Array[Byte](1, 2), Array[Byte](1, 2))
    val trackedB = Seq(Array[Byte](1, 2), Array[Byte](0x80.toByte))
    val miningA = Seq(Array[Byte](5), Array[Byte](4), Array[Byte](5))
    val miningB = Seq(Array[Byte](4), Array[Byte](5))

    definition(trackedA, miningA) shouldBe definition(trackedB, miningB)
  }

  property("tracked and mining script categories remain distinct") {
    val script = Array[Byte](1, 2, 3)

    definition(tracked = Seq(script), mining = Seq.empty, scans = Seq.empty) should not be
      definition(tracked = Seq.empty, mining = Seq(script.clone()), scans = Seq.empty)
  }

  property("external scans are sorted by ID and duplicate IDs fail closed") {
    val low = BaseScan.copy(
      scanId = ScanId @@ 20.toShort,
      walletInteraction = ScanWalletInteraction.Off)
    val high = BaseScan.copy(
      scanId = ScanId @@ 70.toShort,
      trackingRule = OtherPredicate,
      walletInteraction = ScanWalletInteraction.Shared)
    val reverseOrderPayload = UtxoSnapshotScanDefinition.canonicalPayload(
      Seq.empty,
      Seq.empty,
      miningRewardDelay = 0,
      Seq(high, low),
      None).get
    val expectedAscendingPayload =
      "010000000228ff2103" +
        (0 until 32).map(i => f"$i%02x").mkString +
        "8c01fe2103" + List.fill(32)("55").mkString + "00"

    definition(scans = Seq(high, low)) shouldBe definition(scans = Seq(low, high))
    Base16.encode(reverseOrderPayload.toArray) shouldBe expectedAscendingPayload
    calculate(scans = Seq(low, low.copy(scanName = "duplicate"))).isFailure shouldBe true
  }

  property("every result-affecting field changes the definition independently") {
    val base = definition()
    val variants = Seq(
      "scan id" -> definition(scans = Seq(BaseScan.copy(scanId = ScanId @@ 51.toShort))),
      "interaction" -> definition(scans = Seq(BaseScan.copy(walletInteraction = ScanWalletInteraction.Shared))),
      "predicate" -> definition(scans = Seq(BaseScan.copy(trackingRule = OtherPredicate))),
      "tracked script" -> definition(tracked = Seq(Array[Byte](3))),
      "mining script" -> definition(mining = Seq(Array[Byte](3))),
      "reward-delay branch" -> definition(rewardDelay = 0),
      "dust presence" -> definition(dustLimit = Some(1L))
    )

    variants.foreach { case (field, changed) =>
      withClue(field) {
        changed.digest should not be base.digest
      }
    }

    definition(dustLimit = Some(1L)).digest should not be definition(dustLimit = Some(2L)).digest
  }

  property("scan labels and offchain policy do not affect the definition") {
    val renamed = BaseScan.copy(scanName = "renamed")
    val differentOffchainPolicy = BaseScan.copy(removeOffchain = !BaseScan.removeOffchain)

    definition(scans = Seq(renamed)) shouldBe definition(scans = Seq(BaseScan))
    definition(scans = Seq(differentOffchainPolicy)) shouldBe definition(scans = Seq(BaseScan))
  }

  property("a valid Bloom filter extra false positive alone does not affect the definition") {
    val baseCache = WalletCache(defaultProver.hdPubKeys, settings)
    val originalFilter = WalletCache.createScriptsFilter(
      baseCache.trackedBytes, baseCache.miningScriptsBytes, WalletProfile.User)
    val extraEntryFilter = WalletCache.createScriptsFilter(
      baseCache.trackedBytes, baseCache.miningScriptsBytes, WalletProfile.User)
    val falsePositive = Array.fill(64)(0x55.toByte)
    originalFilter.mightContain(falsePositive) shouldBe false
    extraEntryFilter.put(falsePositive) shouldBe true

    (baseCache.trackedBytes ++ baseCache.miningScriptsBytes).foreach { script =>
      originalFilter.mightContain(script) shouldBe true
      extraEntryFilter.mightContain(script) shouldBe true
    }
    extraEntryFilter.mightContain(falsePositive) shouldBe true

    val originalCache = WalletCache(
      baseCache.publicKeyAddresses,
      baseCache.trackedPubKeys,
      baseCache.trackedBytes,
      originalFilter)(settings)
    val extraEntryCache = WalletCache(
      baseCache.publicKeyAddresses,
      baseCache.trackedPubKeys,
      baseCache.trackedBytes,
      extraEntryFilter)(settings)
    val originalVars = WalletVars(None, Seq(BaseScan), Some(originalCache))(settings)
    val extraEntryVars = WalletVars(None, Seq(BaseScan), Some(extraEntryCache))(settings)

    UtxoSnapshotScanDefinition.calculate(originalVars, Some(10L)).get shouldBe
      UtxoSnapshotScanDefinition.calculate(extraEntryVars, Some(10L)).get
  }

  property("a valid Bloom profile change alone does not affect the definition") {
    val baseCache = WalletCache(defaultProver.hdPubKeys, settings)
    val unchangedFilter = WalletCache.createScriptsFilter(
      baseCache.trackedBytes, baseCache.miningScriptsBytes, WalletProfile.User)
    (baseCache.trackedBytes ++ baseCache.miningScriptsBytes).foreach { script =>
      unchangedFilter.mightContain(script) shouldBe true
    }
    val profileOnlySettings = settings.copy(walletSettings = settings.walletSettings.copy(
      profile = WalletProfile.Exchange.label))
    val userCache = WalletCache(
      baseCache.publicKeyAddresses,
      baseCache.trackedPubKeys,
      baseCache.trackedBytes,
      unchangedFilter)(settings)
    val exchangeProfileCache = WalletCache(
      baseCache.publicKeyAddresses,
      baseCache.trackedPubKeys,
      baseCache.trackedBytes,
      unchangedFilter)(profileOnlySettings)
    val userVars = WalletVars(None, Seq(BaseScan), Some(userCache))(settings)
    val exchangeProfileVars = WalletVars(
      None,
      Seq(BaseScan),
      Some(exchangeProfileCache))(profileOnlySettings)

    UtxoSnapshotScanDefinition.calculate(userVars, Some(10L)).get shouldBe
      UtxoSnapshotScanDefinition.calculate(exchangeProfileVars, Some(10L)).get
  }

  property("excluded wallet settings do not affect the definition") {
    val baseCache = WalletCache(defaultProver.hdPubKeys, settings)
    val unchangedFilter = WalletCache.createScriptsFilter(
      baseCache.trackedBytes, baseCache.miningScriptsBytes, WalletProfile.User)
    val excludedSettings = settings.copy(walletSettings = settings.walletSettings.copy(
      keepSpentBoxes = !settings.walletSettings.keepSpentBoxes,
      defaultTransactionFee = settings.walletSettings.defaultTransactionFee + 1,
      maxInputs = settings.walletSettings.maxInputs + 1,
      optimalInputs = settings.walletSettings.optimalInputs + 1))
    val originalCache = WalletCache(
      baseCache.publicKeyAddresses,
      baseCache.trackedPubKeys,
      baseCache.trackedBytes,
      unchangedFilter)(settings)
    val excludedSettingsCache = WalletCache(
      baseCache.publicKeyAddresses,
      baseCache.trackedPubKeys,
      baseCache.trackedBytes,
      unchangedFilter)(excludedSettings)
    val originalVars = WalletVars(None, Seq(BaseScan), Some(originalCache))(settings)
    val excludedSettingsVars = WalletVars(
      None,
      Seq(BaseScan),
      Some(excludedSettingsCache))(excludedSettings)

    UtxoSnapshotScanDefinition.calculate(originalVars, Some(10L)).get shouldBe
      UtxoSnapshotScanDefinition.calculate(excludedSettingsVars, Some(10L)).get
  }

  property("unsigned script ordering and length framing are canonical") {
    val unsignedPayload = UtxoSnapshotScanDefinition.canonicalPayload(
      Seq(Array[Byte](0x80.toByte), Array[Byte](0x7f.toByte)),
      Seq.empty,
      miningRewardDelay = 0,
      Seq.empty,
      None).get

    Base16.encode(unsignedPayload.toArray) shouldBe "0102017f018000000000"

    definition(
      tracked = Seq(Array[Byte](1), Array[Byte](2, 3)),
      mining = Seq.empty,
      scans = Seq.empty) should not be definition(
      tracked = Seq(Array[Byte](1, 2), Array[Byte](3)),
      mining = Seq.empty,
      scans = Seq.empty)
  }

  property("golden payload and digest lock domain, framing, ordering, and predicate serialization") {
    val tracked = Seq(Array[Byte](2, 3), Array[Byte](1))
    val mining = Seq(Array[Byte](4, 5))
    val scan = BaseScan.copy(
      scanId = ScanId @@ 258.toShort,
      walletInteraction = ScanWalletInteraction.Forced)
    val expectedPayload =
      "010201010202030102040501018404fd2103" +
        (0 until 32).map(i => f"$i%02x").mkString +
        "01909cb0d080c1818202"

    val payload = UtxoSnapshotScanDefinition.canonicalPayload(
      tracked,
      mining,
      miningRewardDelay = 1,
      Seq(scan),
      Some(0x0102030405060708L)).get
    val calculated = definition(
      tracked,
      mining,
      rewardDelay = 1,
      Seq(scan),
      Some(0x0102030405060708L))

    Base16.encode(payload.toArray) shouldBe expectedPayload
    Base16.encode(calculated.digest.toArray) shouldBe
      "2499e567e7d7cc797d71dbd7a71542d0c10fceaebff1f36264882a25c58e404d"
  }

  property("predicate serialization failure is returned as Failure") {
    val malformed = BaseScan.copy(
      trackingRule = AndScanningPredicate(null.asInstanceOf[ScanningPredicate]))

    calculate(scans = Seq(malformed)).isFailure shouldBe true
  }
}
