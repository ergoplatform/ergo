package org.ergoplatform.nodeView.state

import org.ergoplatform.core.idToVersion
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}
import scorex.crypto.authds.ADDigest
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, InternalProverNode, PersistentBatchAVLProver, ProverNodes, VersionedLDBAVLStorage}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

class UtxoStateRollbackSpecification extends ErgoCorePropertyTest {

  for (wrongHeight <- Seq(false, true)) {
    property(s"rollback rejects a recovered root with the wrong ${if (wrongHeight) "height" else "hash"}") {
      val (original, _) = createUtxoState(settings)
      try {
        val expected = original.rootDigest.clone()
        val originalProver = original.persistentProver.avlProver
        val originalRoot = original.persistentProver.storage.rollback(original.rootDigest).get
        val recovered = if (wrongHeight) {
          originalRoot._1 -> (originalRoot._2 + 1)
        } else {
          originalRoot._1.asInstanceOf[InternalProverNode[Digest32]].left -> originalRoot._2
        }
        val recoveredStorage = new VersionedLDBAVLStorage(original.store) {
          override def rollback(version: ADDigest): Try[(ProverNodes[Digest32], Int)] = {
            version shouldEqual expected
            Success(recovered)
          }
        }
        // Exercise the dependency's real rollback, which installs the storage result.
        val prover = new PersistentBatchAVLProver[Digest32, HF] {
          override var avlProver: BatchAVLProver[Digest32, HF] = originalProver
          override val storage: VersionedLDBAVLStorage = recoveredStorage
        }
        val state = new UtxoState(prover, original.version, original.store, settings)

        val result = state.rollbackTo(original.version)

        prover.digest.sameElements(expected) shouldBe false
        prover.digest.dropRight(1).sameElements(expected.dropRight(1)) shouldBe wrongHeight
        (prover.digest.last == expected.last) shouldBe !wrongHeight
        result.isFailure shouldBe true
        result.failed.get shouldBe a[IllegalArgumentException]
        result.failed.get.getMessage should include("digest")
      } finally original.closeStorage()
    }
  }

  property("rollback propagates a prover failure without publishing the requested version") {
    val (original, _) = createUtxoState(settings)
    try {
      val expected = original.rootDigest.clone()
      val failure = new IllegalStateException("local prover rollback failure")
      val prover = new PersistentBatchAVLProver[Digest32, HF] {
        override var avlProver: BatchAVLProver[Digest32, HF] = original.persistentProver.avlProver
        override val storage: VersionedLDBAVLStorage = new VersionedLDBAVLStorage(original.store) {
          override def rollback(version: ADDigest): Try[(ProverNodes[Digest32], Int)] = Failure(failure)
        }
      }
      val state = new UtxoState(prover, original.version, original.store, settings)

      val result = state.rollbackTo(original.version)

      result.failed.get should be theSameInstanceAs failure
      prover.digest shouldEqual expected
    } finally original.closeStorage()
  }

  property("rollback publishes the requested version only with its stored root") {
    val (original, boxes) = createUtxoState(settings)
    try {
      val expected = original.rootDigest.clone()
      val block = validFullBlock(None, original, boxes)
      val advanced = original.applyModifier(block, None)(_ => ()).get
      advanced.version shouldEqual idToVersion(block.id)
      advanced.rootDigest.sameElements(expected) shouldBe false

      val rolledBack = advanced.rollbackTo(original.version).get

      rolledBack.version shouldEqual original.version
      rolledBack.rootDigest shouldEqual expected
      rolledBack.rollbackTo(rolledBack.version).get.rootDigest shouldEqual expected
    } finally original.closeStorage()
  }
}
