package org.ergoplatform.utils.generators

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest, UtxoSnapshotManifestSerializer}
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils.ErgoTestConstants
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer, BatchAVLProverSubtree}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.testkit.generators.CoreGenerators

trait UtxoStateGenerators extends CoreGenerators with Matchers with ErgoTestConstants  {

  private implicit val hf: Algos.HF = Algos.hash
  private val serializer = new BatchAVLProverSerializer[Digest32, Algos.HF]

  lazy val proverGen: Gen[BatchAVLProver[Digest32, Algos.HF]] = for {
    treeSize <- Gen.chooseNum(100, 1000)
  } yield {
    val prover = new BatchAVLProver[Digest32, Algos.HF](Constants.HashLength, None)
    val values = (0 until treeSize).map { i =>
      val data = Ints.toByteArray(i)
      (ADKey !@@ Blake2b256(data), ADValue @@ data)
    }
    values.foreach(kv => prover.performOneOperation(Insert(kv._1, kv._2)))
    prover.generateProof()
    prover
  }

  lazy val proverManifestGen: Gen[BatchAVLProverManifest[Digest32, Algos.HF]] = for {
    tree <- proverGen
  } yield serializer.slice(tree)._1

  lazy val proverSubtreeGen: Gen[BatchAVLProverSubtree[Digest32, Algos.HF]] = for {
    tree <- proverGen
  } yield serializer.slice(tree)._2.head

  lazy val validUtxoSnapshotGen: Gen[UtxoSnapshot] = for {
    tree <- proverGen
    blockId <- modifierIdGen
    height <- Gen.negNum[Int]
  } yield {
    val (proverManifest, proverSubtrees) = serializer.slice(tree)
    val chunks = proverSubtrees
      .grouped(Constants.UtxoChunkCapacity)
      .toIndexedSeq
      .zipWithIndex
      .map { case (trees, idx) =>
        UtxoSnapshotChunk(trees.toIndexedSeq, idx)
      }
    val manifest = UtxoSnapshotManifest(chunks.map(_.rootHash), blockId, height, proverManifest)
    UtxoSnapshot(manifest, chunks)
  }

  lazy val randomUtxoSnapshotChunkGen: Gen[UtxoSnapshotChunk] = for {
    index <- Arbitrary.arbitrary[Short]
    stateElements <- Gen.listOf(proverSubtreeGen)
  } yield UtxoSnapshotChunk(stateElements.toIndexedSeq, index)

  lazy val randomUtxoSnapshotManifestGen: Gen[UtxoSnapshotManifest] = for {
    chunksQty <- Gen.chooseNum(1, 100)
    chunkRootHashes <- Gen.listOfN(chunksQty, genBytes(UtxoSnapshotManifestSerializer.rootHashSize))
    proverManifest <- proverManifestGen
    blockId <- modifierIdGen
    height <- Gen.negNum[Int]
  } yield UtxoSnapshotManifest(chunkRootHashes.toIndexedSeq, blockId, height, proverManifest)

}
