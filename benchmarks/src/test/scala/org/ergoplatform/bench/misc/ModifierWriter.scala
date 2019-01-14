package org.ergoplatform.bench.misc

import java.io.{InputStream, OutputStream}

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history._
import scorex.core.serialization.Serializer
import scorex.core.{ModifierTypeId, NodeViewModifier}

object ModifierWriter {

  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: ErgoPersistentModifier]] =
    Map(Header.modifierTypeId -> HeaderSerializer,
      BlockTransactions.modifierTypeId -> BlockTransactionsSerializer,
      ADProofs.modifierTypeId -> ADProofSerializer)

  def write(mod: NodeViewModifier)(implicit fos: OutputStream): Unit = {
    val typeId = mod.modifierTypeId
    val bytes = mod.bytes
    val length: Int = bytes.length
    val lengthBytes = Ints.toByteArray(length)
    val bytesToWrite = typeId +: (lengthBytes ++ bytes)
    fos.write(bytesToWrite)
    fos.flush()
  }

  def read(implicit fis: InputStream): Option[ErgoPersistentModifier] = for {
    typeId <- readModId
    length <- readLength
    bytes <- readBytes(length)
    mod <- modifierSerializers(typeId).parseBytes(bytes).toOption
  } yield mod

  private def readModId(implicit fis: InputStream): Option[ModifierTypeId] = {
    val int = fis.read()
    if (int == -1) { None } else { Some(ModifierTypeId @@ int.toByte) }
  }

  private def readLength(implicit fis: InputStream): Option[Int] =
    Some(Stream.continually(fis.read().toByte).take(4).toArray).map(Ints.fromByteArray)

  private def readBytes(length: Int)(implicit fis: InputStream): Option[Array[Byte]] =
    Some(Stream.continually(fis.read().toByte).take(length).toArray)

}
