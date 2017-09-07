package org.ergoplatform.modifiers.state

import io.circe.Json
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32

import scala.util.Try

case class UTXOSnapshotManifest(chunkRootHashes: Seq[Array[Byte]], blockId: ModifierId) extends ErgoPersistentModifier {
  override val modifierTypeId: ModifierTypeId = UTXOSnapshotManifest.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ Algos.hash(scorex.core.utils.concatBytes(chunkRootHashes :+ blockId))

  override type M = UTXOSnapshotManifest

  override lazy val serializer: Serializer[UTXOSnapshotManifest] = ???

  override lazy val json: Json = ???

  //todo: asInstanceOf
  lazy val rootHash: Digest32 = Algos.merkleTreeRoot(chunkRootHashes.asInstanceOf[Seq[LeafData]])
}

object UTXOSnapshotManifest {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (106: Byte)

  def validate(manifest: UTXOSnapshotManifest, header: Header): Try[Unit] = Try {
    require(manifest.blockId sameElements header.id)
    require(manifest.rootHash sameElements header.stateRoot)
    ???
  }
}

