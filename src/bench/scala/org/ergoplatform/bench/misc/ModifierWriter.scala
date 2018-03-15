package org.ergoplatform.bench.misc

import java.io.{InputStream, OutputStream}

import com.google.common.primitives.Ints
import scorex.core.{ModifierTypeId, NodeViewModifier}

object ModifierWriter {

  def write(mod: NodeViewModifier)(implicit fos: OutputStream): Unit = {
    val typeId = mod.modifierTypeId
    val bytes = mod.bytes
    val length: Int = bytes.length
    val lengthBytes = Ints.toByteArray(length)
    val bytesToWrite = typeId +: (lengthBytes ++ bytes)
    fos.write(bytesToWrite)
    fos.flush()
  }

  def read(implicit fis: InputStream): Option[(ModifierTypeId, Array[Byte])] = for {
    typeId <- readModId
    length <- readLength
    bytes <- readBytes(length)
  } yield (typeId, bytes)

  private def readModId(implicit fis: InputStream): Option[ModifierTypeId] =
    Some(ModifierTypeId @@ fis.read().toByte).filterNot(_ == -1)

  private def readLength(implicit fis: InputStream): Option[Int] =
    Some(Stream.continually(fis.read().toByte).take(4).toArray).filterNot(_.contains(-1: Byte)).map(Ints.fromByteArray)

  private def readBytes(length: Int)(implicit fis: InputStream): Option[Array[Byte]] =
    Some(Stream.continually(fis.read().toByte).take(length).toArray)

}
