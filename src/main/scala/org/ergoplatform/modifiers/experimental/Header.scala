package org.ergoplatform.modifiers.experimental

import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

case class Header(version: Version,
                  parentId: BlockId,
                  interlinksRoot: Array[Byte],
                  ADProofsRoot: Array[Byte],
                  stateRoot: Array[Byte],
                  transactionsRoot: Array[Byte],
                  timestamp: Block.Timestamp,
                  nonce: Int) extends ErgoModifier {

  lazy val payloadRootHash: Array[Byte] = Algos.merkleTreeRoot(Seq(Array(version),
    interlinksRoot,
    ADProofsRoot,
    stateRoot,
    transactionsRoot,
    Longs.toByteArray(timestamp),
    parentId
  ))

  lazy val minimalHeader = MinimalHeader(payloadRootHash, nonce)

  override val modifierTypeId: ModifierTypeId = Header.ModifierTypeId

  override lazy val id: ModifierId = minimalHeader.id

  lazy val realDifficulty: BigInt = Algos.blockIdDifficulty(id)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "transactionsRoot" -> Base58.encode(transactionsRoot).asJson,
    "interlinksRoot" -> Base58.encode(interlinksRoot).asJson,
    "ADProofsRoot" -> Base58.encode(ADProofsRoot).asJson,
    "stateRoot" -> Base58.encode(stateRoot).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson
  ).asJson

  override lazy val toString: String = s"Header(${json.noSpaces})"

  override type M = Header

  override lazy val serializer: Serializer[Header] = ???

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Header => id sameElements that.id
    case _ => false
  }

  lazy val isGenesis: Boolean = interlinksRoot sameElements Algos.EmptyMerkleTreeRoot
}

object Header {
  val ModifierTypeId: Byte = 101: Byte
}
