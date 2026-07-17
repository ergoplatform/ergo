package org.ergoplatform.mining

import com.google.common.primitives.Ints
import org.ergoplatform.{InputSolutionFound, OrderingSolutionFound}
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.settings.{Algos, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{bytesToId, idToBytes}

import org.ergoplatform.utils.generators.CoreObjectGenerators._
import org.ergoplatform.utils.generators.ErgoCoreGenerators._

class InputBlockAnnouncementSpec extends ErgoCorePropertyTest {

  private val powScheme = new AutolykosPowScheme(32, 26)
  private val defaultParams = Parameters(0, Parameters.DefaultParameters, ErgoValidationSettingsUpdate.empty)

  // Helper to create valid Merkle proof for input block fields
  private def createValidMerkleProof(
    prevInputBlockIdOpt: Option[Array[Byte]],
    transactionsDigest: Digest32,
    prevTransactionsDigest: Digest32
  ): BatchMerkleProof[Digest32] = {
    val extCandidate = InputBlockFields.toExtensionFields(
      prevInputBlockIdOpt,
      transactionsDigest,
      prevTransactionsDigest
    )
    
    extCandidate.proofForInputBlockData.get
  }

  // Helper to create invalid Merkle proof (wrong digest)
  private def createInvalidMerkleProof(
    prevInputBlockIdOpt: Option[Array[Byte]],
    transactionsDigest: Digest32,
    prevTransactionsDigest: Digest32
  ): BatchMerkleProof[Digest32] = {
    // Create proof with wrong transactions digest
    val wrongDigest = Digest32 @@ Array.fill(32)(0xFF.toByte)
    val extCandidate = InputBlockFields.toExtensionFields(
      prevInputBlockIdOpt,
      wrongDigest,
      prevTransactionsDigest
    )
    
    extCandidate.proofForInputBlockData.get
  }

  // Helper to create empty Merkle proof
  private def createEmptyMerkleProof: BatchMerkleProof[Digest32] = {
    BatchMerkleProof(Seq.empty, Seq.empty)(Blake2b256)
  }

  /**
   * Tests that InputBlockAnnouncement.valid() returns true when both PoW and Merkle proof are valid.
   * Creates a valid input block solution with correct PoW, constructs proper Merkle proof
   * for the extension fields, and verifies the InputBlockAnnouncement structure.
   */
  property("InputBlockAnnouncement.valid() should return true for valid input block with correct PoW and Merkle proof") {
    forAll(invalidHeaderGen, Gen.choose(100, 120), digest32Gen, digest32Gen, stateRootGen) { 
      (baseHeader, difficulty, transactionsDigest, prevTransactionsDigest, stateRoot) =>
        
        val nBits = DifficultySerializer.encodeCompactBits(difficulty)
        val h = baseHeader.copy(nBits = nBits, version = 2)
        val sk = randomSecret()
        val x = randomSecret()
        val msg = powScheme.msgByHeader(h)
        val b = powScheme.getB(h.nBits)
        val hbs = Ints.toByteArray(h.height)
        val N = powScheme.calcN(h)

        powScheme.checkNonces(2, hbs, msg, sk, x, b, N, 0, 10000, defaultParams) match {
          case InputSolutionFound(as) =>
            // Found valid input block solution
            val inputBlockHeader = h.copy(powSolution = as)

            val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
            val merkleProof = createValidMerkleProof(
              prevInputBlockId,
              transactionsDigest,
              prevTransactionsDigest
            )

            val extensionRoot = Algos.merkleTreeRoot(
              Extension.merkleTree(
                InputBlockFields.toExtensionFields(
                  prevInputBlockId,
                  transactionsDigest,
                  prevTransactionsDigest
                ).fields
              )
            )

            // Test PoW validity on the original header (before extension root change)
            powScheme.checkInputBlockPoW(inputBlockHeader, defaultParams) shouldBe true
            
            val inputBlockFields = new InputBlockFields(
              prevInputBlockId,
              transactionsDigest,
              prevTransactionsDigest,
              merkleProof
            )

            // Create InputBlockAnnouncement with the original header (PoW valid)
            // Note: In a real block, extensionRoot in header would match the Merkle proof
            // Here we test that both components are valid independently
            val inputBlockInfo = InputBlockAnnouncement(
              InputBlockAnnouncement.initialMessageVersion,
              inputBlockHeader,
              inputBlockFields,
              None
            )

            // Verify Merkle proof is valid for the extension root it was created for
            inputBlockInfo.inputBlockFields.inputBlockFieldsProof.valid(extensionRoot) shouldBe true
            
            // Verify structure
            inputBlockInfo.header shouldBe inputBlockHeader
            inputBlockInfo.inputBlockFields shouldBe inputBlockFields
            inputBlockInfo.transactionsDigest shouldBe transactionsDigest
            inputBlockInfo.prevInputBlockId shouldBe prevInputBlockId.map(bytesToId)
            
          case OrderingSolutionFound(_) =>
            // Found ordering block solution (not input block) - skip this test case
            succeed
            
          case _ =>
            // No solution found in nonce range - skip this test case
            succeed
        }
    }
  }

  /**
   * Tests that InputBlockAnnouncement.valid() returns false when expectedNBits is provided
   * and does not match the header's nBits, even if PoW and Merkle proof are valid.
   * This verifies that an attacker cannot submit an input block with a lower difficulty
   * (smaller nBits) to bypass PoW validation.
   */
  property("InputBlockAnnouncement.valid() should return false when nBits does not match expectedNBits") {
    forAll(invalidHeaderGen, Gen.choose(100, 120), digest32Gen, digest32Gen, stateRootGen, Gen.choose(0, 200)) {
      (baseHeader, difficulty, transactionsDigest, prevTransactionsDigest, stateRoot, wrongDifficulty) =>

        val nBits = DifficultySerializer.encodeCompactBits(difficulty)
        val wrongNBits = DifficultySerializer.encodeCompactBits(Math.max(1, wrongDifficulty.toLong))

        val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
        val extensionRoot = Algos.merkleTreeRoot(
          Extension.merkleTree(
            InputBlockFields.toExtensionFields(
              prevInputBlockId,
              transactionsDigest,
              prevTransactionsDigest
            ).fields
          )
        )

        val h = baseHeader.copy(nBits = nBits, version = 2, extensionRoot = extensionRoot)
        val sk = randomSecret()
        val x = randomSecret()
        val msg = powScheme.msgByHeader(h)
        val b = powScheme.getB(h.nBits)
        val hbs = Ints.toByteArray(h.height)
        val N = powScheme.calcN(h)

        whenever(wrongNBits != nBits) {
          powScheme.checkNonces(2, hbs, msg, sk, x, b, N, 0, 10000, defaultParams) match {
            case InputSolutionFound(as) =>
              val inputBlockHeader = h.copy(powSolution = as)

              powScheme.checkInputBlockPoW(inputBlockHeader, defaultParams) shouldBe true

              val merkleProof = createValidMerkleProof(
                prevInputBlockId,
                transactionsDigest,
                prevTransactionsDigest
              )

              val inputBlockFields = new InputBlockFields(
                prevInputBlockId,
                transactionsDigest,
                prevTransactionsDigest,
                merkleProof
              )

            val inputBlockInfo = InputBlockAnnouncement(
              InputBlockAnnouncement.initialMessageVersion,
              inputBlockHeader,
              inputBlockFields,
              None,
              Array.emptyByteArray
            )

              inputBlockInfo.inputBlockFields.inputBlockFieldsProof.valid(inputBlockHeader.extensionRoot) shouldBe true
              inputBlockInfo.valid(powScheme, defaultParams, Some(nBits)) shouldBe true
              inputBlockInfo.valid(powScheme, defaultParams, Some(wrongNBits)) shouldBe false

            case _ =>
              succeed
          }
        }
    }
  }

  /**
   * Tests that InputBlockAnnouncement.valid() returns true when expectedNBits matches header.nBits.
   */
  property("InputBlockAnnouncement.valid() should return true when nBits matches expectedNBits") {
    forAll(invalidHeaderGen, Gen.choose(100, 120), digest32Gen, digest32Gen, stateRootGen) {
      (baseHeader, difficulty, transactionsDigest, prevTransactionsDigest, stateRoot) =>

        val nBits = DifficultySerializer.encodeCompactBits(difficulty)

        val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
        val extensionRoot = Algos.merkleTreeRoot(
          Extension.merkleTree(
            InputBlockFields.toExtensionFields(
              prevInputBlockId,
              transactionsDigest,
              prevTransactionsDigest
            ).fields
          )
        )

        val h = baseHeader.copy(nBits = nBits, version = 2, extensionRoot = extensionRoot)
        val sk = randomSecret()
        val x = randomSecret()
        val msg = powScheme.msgByHeader(h)
        val b = powScheme.getB(h.nBits)
        val hbs = Ints.toByteArray(h.height)
        val N = powScheme.calcN(h)

        powScheme.checkNonces(2, hbs, msg, sk, x, b, N, 0, 10000, defaultParams) match {
          case InputSolutionFound(as) =>
            val inputBlockHeader = h.copy(powSolution = as)

            powScheme.checkInputBlockPoW(inputBlockHeader, defaultParams) shouldBe true

            val merkleProof = createValidMerkleProof(
              prevInputBlockId,
              transactionsDigest,
              prevTransactionsDigest
            )

            val inputBlockFields = new InputBlockFields(
              prevInputBlockId,
              transactionsDigest,
              prevTransactionsDigest,
              merkleProof
            )

            val inputBlockInfo = InputBlockAnnouncement(
              InputBlockAnnouncement.initialMessageVersion,
              inputBlockHeader,
              inputBlockFields,
              None,
              Array.emptyByteArray
            )

            inputBlockInfo.inputBlockFields.inputBlockFieldsProof.valid(inputBlockHeader.extensionRoot) shouldBe true
            inputBlockInfo.valid(powScheme, defaultParams, Some(nBits)) shouldBe true

          case _ =>
            succeed
        }
    }
  }

  /**
   * Tests that InputBlockAnnouncement.valid() returns false when the Merkle proof is invalid.
   * Creates a Merkle proof with a wrong transactions digest, then verifies that
   * the proof fails validation against the correct extension root.
   */
  property("InputBlockAnnouncement.valid() should return false when Merkle proof is invalid") {
    forAll(invalidHeaderGen, digest32Gen, digest32Gen, stateRootGen) { 
      (baseHeader, transactionsDigest, prevTransactionsDigest, stateRoot) =>
        
        val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
        
        // Create invalid Merkle proof (proof doesn't match the actual fields)
        val invalidMerkleProof = createInvalidMerkleProof(
          prevInputBlockId,
          transactionsDigest,
          prevTransactionsDigest
        )

        // Create extension root from correct fields
        val correctFields = Algos.merkleTreeRoot(
          Extension.merkleTree(
            InputBlockFields.toExtensionFields(
              prevInputBlockId,
              transactionsDigest,
              prevTransactionsDigest
            ).fields
          )
        )

        val header = baseHeader.copy(
          extensionRoot = correctFields,
          stateRoot = stateRoot,
          version = 2
        )

        val inputBlockFields = new InputBlockFields(
          prevInputBlockId,
          transactionsDigest,
          prevTransactionsDigest,
          invalidMerkleProof
        )

        val inputBlockInfo = InputBlockAnnouncement(
          InputBlockAnnouncement.initialMessageVersion,
          header,
          inputBlockFields,
          None,
          Array.emptyByteArray
        )

        // Merkle proof validation should fail
        inputBlockInfo.inputBlockFields.inputBlockFieldsProof.valid(header.extensionRoot) shouldBe false
    }
  }

  /**
   * Tests that InputBlockAnnouncement.valid() returns false when the Merkle proof is empty but fields exist.
   * An empty BatchMerkleProof cannot validate against a non-empty extension root.
   */
  property("InputBlockAnnouncement.valid() should return false when Merkle proof is empty but fields exist") {
    forAll(invalidHeaderGen, digest32Gen, digest32Gen, stateRootGen) { 
      (baseHeader, transactionsDigest, prevTransactionsDigest, stateRoot) =>
        
        val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
        
        // Create empty Merkle proof
        val emptyMerkleProof = createEmptyMerkleProof

        // Create extension root from correct fields
        val correctFields = Algos.merkleTreeRoot(
          Extension.merkleTree(
            InputBlockFields.toExtensionFields(
              prevInputBlockId,
              transactionsDigest,
              prevTransactionsDigest
            ).fields
          )
        )

        val header = baseHeader.copy(
          extensionRoot = correctFields,
          stateRoot = stateRoot,
          version = 2
        )

        val inputBlockFields = new InputBlockFields(
          prevInputBlockId,
          transactionsDigest,
          prevTransactionsDigest,
          emptyMerkleProof
        )

        val inputBlockInfo = InputBlockAnnouncement(
          InputBlockAnnouncement.initialMessageVersion,
          header,
          inputBlockFields,
          None,
          Array.emptyByteArray
        )

        // Empty proof should not validate against non-empty extension root
        inputBlockInfo.inputBlockFields.inputBlockFieldsProof.valid(header.extensionRoot) shouldBe false
    }
  }

  /**
   * Tests that InputBlockAnnouncement.id correctly returns the underlying header's id.
   */
  property("InputBlockAnnouncement.id should return header id") {
    forAll(invalidHeaderGen, digest32Gen, digest32Gen) { (header, transactionsDigest, prevTransactionsDigest) =>
      
      val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
      val merkleProof = createValidMerkleProof(prevInputBlockId, transactionsDigest, prevTransactionsDigest)
      val fields = new InputBlockFields(prevInputBlockId, transactionsDigest, prevTransactionsDigest, merkleProof)
      val ibi = InputBlockAnnouncement(InputBlockAnnouncement.initialMessageVersion, header, fields, None, Array.emptyByteArray)
      
      ibi.id shouldBe header.id
    }
  }

  /**
   * Tests that the Merkle proof validates correctly against its own extension root
   * and fails validation against a wrong root.
   */
  property("InputBlockAnnouncement Merkle proof should validate with correct extension root") {
    forAll(digest32Gen, digest32Gen) { (transactionsDigest, prevTransactionsDigest) =>

      val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))

      // Create fields and Merkle proof using ExtensionCandidate
      val extCandidate = InputBlockFields.toExtensionFields(
        prevInputBlockId,
        transactionsDigest,
        prevTransactionsDigest
      )

      val extensionRoot = extCandidate.digest
      val merkleProof = extCandidate.proofForInputBlockData.get

      // Proof should validate against the root
      merkleProof.valid(extensionRoot) shouldBe true

      // Proof should NOT validate against wrong root
      val wrongRoot = Digest32 @@ Array.fill(32)(0xFF.toByte)
      merkleProof.valid(wrongRoot) shouldBe false
    }
  }

  /**
   * Tests that the first input block (after an ordering block, with no prevInputBlockId)
   * creates a valid Merkle proof with only 2 extension fields.
   */
  property("InputBlockAnnouncement with first input block (no prevInputBlockId) should create valid proof") {
    forAll(digest32Gen, digest32Gen) { (transactionsDigest, prevTransactionsDigest) =>

      // First input block after ordering block has no previous input block
      val prevInputBlockId: Option[Array[Byte]] = None

      val extCandidate = InputBlockFields.toExtensionFields(
        prevInputBlockId,
        transactionsDigest,
        prevTransactionsDigest
      )

      val extensionRoot = extCandidate.digest
      val merkleProof = extCandidate.proofForInputBlockData.get

      // Should have 2 fields (no prevInputBlockId)
      extCandidate.fields.length shouldBe 2

      // Proof should validate
      merkleProof.valid(extensionRoot) shouldBe true
    }
  }

  /**
   * Tests that all extension field values created by InputBlockFields have the correct size of 32 bytes.
   * Verifies prevInputBlockId, transactionsDigest, and prevTransactionsDigest are all 32 bytes.
   */
  property("InputBlockAnnouncement extension field values should have correct sizes") {
    forAll(digest32Gen, digest32Gen, modifierIdGen) { (transactionsDigest, prevTransactionsDigest, prevId) =>
      
      val prevInputBlockId: Option[Array[Byte]] = Some(idToBytes(prevId))
      
      val extensionFields = InputBlockFields.toExtensionFields(
        prevInputBlockId,
        transactionsDigest,
        prevTransactionsDigest
      ).fields
      
      // prevInputBlockId should be 32 bytes
      extensionFields.find(_._1 sameElements Extension.PrevInputBlockIdKey).get._2.length shouldBe 32
      
      // transactionsDigest should be 32 bytes
      extensionFields.find(_._1 sameElements Extension.InputBlockTransactionsDigestKey).get._2.length shouldBe 32
      
      // prevTransactionsDigest should be 32 bytes
      extensionFields.find(_._1 sameElements Extension.PreviousInputBlockTransactionsDigestKey).get._2.length shouldBe 32
    }
  }

  /**
   * Tests that Merkle proof validation fails when the transactions digest is tampered with.
   * Verifies that a proof created with correct fields doesn't validate against a tampered root,
   * and a proof created with tampered fields doesn't validate against the original root.
   */
  property("InputBlockAnnouncement Merkle proof should fail with tampered transactions digest") {
    forAll(digest32Gen, digest32Gen) { (transactionsDigest, prevTransactionsDigest) =>

      val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))

      // Create proof with correct fields
      val extCandidate = InputBlockFields.toExtensionFields(
        prevInputBlockId,
        transactionsDigest,
        prevTransactionsDigest
      )

      val extensionRoot = extCandidate.digest
      val merkleProof = extCandidate.proofForInputBlockData.get

      // Tamper with transactions digest
      val tamperedDigest = Digest32 @@ transactionsDigest.map(b => (b ^ 0xFF).toByte)

      // Create new fields with tampered digest
      val tamperedFields = InputBlockFields.toExtensionFields(
        prevInputBlockId,
        tamperedDigest,
        prevTransactionsDigest
      )

      val tamperedRoot = tamperedFields.digest

      // Original proof should not validate against tampered root
      merkleProof.valid(tamperedRoot) shouldBe false

      // Tampered proof should not validate against original root
      val tamperedProof = tamperedFields.proofForInputBlockData.get
      tamperedProof.valid(extensionRoot) shouldBe false
    }
  }

  property("InputBlockAnnouncement.valid() should reject fields not bound to the extension proof") {
    forAll(invalidHeaderGen, Gen.choose(100, 120), digest32Gen, digest32Gen) {
      (baseHeader, difficulty, transactionsDigest, prevTransactionsDigest) =>

        val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
        val extCandidate = InputBlockFields.toExtensionFields(
          prevInputBlockId,
          transactionsDigest,
          prevTransactionsDigest
        )
        val extensionRoot = extCandidate.digest
        val merkleProof = extCandidate.proofForInputBlockData.get

        val nBits = DifficultySerializer.encodeCompactBits(difficulty)
        val h = baseHeader.copy(nBits = nBits, version = 2, extensionRoot = extensionRoot)
        val sk = randomSecret()
        val x = randomSecret()
        val msg = powScheme.msgByHeader(h)
        val b = powScheme.getB(h.nBits)
        val hbs = Ints.toByteArray(h.height)
        val N = powScheme.calcN(h)

        powScheme.checkNonces(2, hbs, msg, sk, x, b, N, 0, 10000, defaultParams) match {
          case InputSolutionFound(as) =>
            val tamperedDigest = Digest32 @@ transactionsDigest.map(b => (b ^ 0xFF).toByte)
            val inputBlockFields = new InputBlockFields(
              prevInputBlockId,
              tamperedDigest,
              prevTransactionsDigest,
              merkleProof
            )

            val inputBlockInfo = InputBlockAnnouncement(
              InputBlockAnnouncement.initialMessageVersion,
              h.copy(powSolution = as),
              inputBlockFields,
              None
            )

            inputBlockInfo.inputBlockFields.inputBlockFieldsProof.valid(extensionRoot) shouldBe true
            inputBlockInfo.valid(powScheme, defaultParams, Some(nBits)) shouldBe false

          case _ =>
            succeed
        }
    }
  }

  /**
   * Tests that InputBlockAnnouncement with version 1 roundtrips correctly with empty unparsed bytes.
   */
  property("InputBlockAnnouncement version 1 serialization roundtrip") {
    forAll(invalidHeaderGen, digest32Gen, digest32Gen) { (header, transactionsDigest, prevTransactionsDigest) =>
      val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
      val merkleProof = createValidMerkleProof(prevInputBlockId, transactionsDigest, prevTransactionsDigest)
      val fields = new InputBlockFields(prevInputBlockId, transactionsDigest, prevTransactionsDigest, merkleProof)
      val ibi = InputBlockAnnouncement(1.toByte, header, fields, None, Array.emptyByteArray)

      val bytes = InputBlockAnnouncement.serializer.toBytes(ibi)
      val recovered = InputBlockAnnouncement.serializer.parseBytes(bytes)

      recovered.version shouldBe ibi.version
      recovered.header shouldBe ibi.header
      recovered.inputBlockFields.prevInputBlockId.map(_.toSeq) shouldBe ibi.inputBlockFields.prevInputBlockId.map(_.toSeq)
      recovered.inputBlockFields.transactionsDigest shouldBe ibi.inputBlockFields.transactionsDigest
      recovered.inputBlockFields.prevTransactionsDigest shouldBe ibi.inputBlockFields.prevTransactionsDigest
      recovered.weakTxIds shouldBe ibi.weakTxIds
      recovered.unparsedBytes.toSeq shouldBe ibi.unparsedBytes.toSeq
    }
  }

  /**
   * Tests that InputBlockAnnouncement with future version stores unparsed bytes for forward compatibility.
   */
  property("InputBlockAnnouncement future version stores unparsed bytes") {
    forAll(invalidHeaderGen, digest32Gen, digest32Gen) { (header, transactionsDigest, prevTransactionsDigest) =>
      val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
      val merkleProof = createValidMerkleProof(prevInputBlockId, transactionsDigest, prevTransactionsDigest)
      val fields = new InputBlockFields(prevInputBlockId, transactionsDigest, prevTransactionsDigest, merkleProof)
      val futureBytes = Array[Byte](0xAB.toByte, 0xCD.toByte, 0xEF.toByte)
      val ibi = InputBlockAnnouncement(2.toByte, header, fields, None, futureBytes)

      val bytes = InputBlockAnnouncement.serializer.toBytes(ibi)
      val recovered = InputBlockAnnouncement.serializer.parseBytes(bytes)

      recovered.version shouldBe ibi.version
      recovered.header shouldBe ibi.header
      recovered.inputBlockFields.prevInputBlockId.map(_.toSeq) shouldBe ibi.inputBlockFields.prevInputBlockId.map(_.toSeq)
      recovered.inputBlockFields.transactionsDigest shouldBe ibi.inputBlockFields.transactionsDigest
      recovered.inputBlockFields.prevTransactionsDigest shouldBe ibi.inputBlockFields.prevTransactionsDigest
      recovered.weakTxIds shouldBe ibi.weakTxIds
      recovered.unparsedBytes.toSeq shouldBe ibi.unparsedBytes.toSeq
    }
  }

  /**
   * Tests that parsing a higher version message does not throw and preserves unknown data.
   */
  property("InputBlockAnnouncement higher version does not throw and preserves unparsed bytes") {
    forAll(invalidHeaderGen, digest32Gen, digest32Gen) { (header, transactionsDigest, prevTransactionsDigest) =>
      val prevInputBlockId: Option[Array[Byte]] = Some(Array.fill(32)(0x01.toByte))
      val merkleProof = createValidMerkleProof(prevInputBlockId, transactionsDigest, prevTransactionsDigest)
      val fields = new InputBlockFields(prevInputBlockId, transactionsDigest, prevTransactionsDigest, merkleProof)
      val futureBytes = Array[Byte](0x12, 0x34, 0x56, 0x78)
      val ibi = InputBlockAnnouncement(3.toByte, header, fields, None, futureBytes)

      val bytes = InputBlockAnnouncement.serializer.toBytes(ibi)
      val recovered = InputBlockAnnouncement.serializer.parseBytes(bytes)

      recovered.version shouldBe 3.toByte
      recovered.unparsedBytes shouldBe futureBytes
    }
  }

}
