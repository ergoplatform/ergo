package org.ergoplatform.utils

import java.io.InputStream
import java.net.URL

import com.google.common.primitives.Ints
import javax.net.ssl.HttpsURLConnection
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history._
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer

trait RealModifiers {

  protected val fileName = "https://github.com/ergoplatform/static-data/raw/master/blocks.dat"

  def takeHeaders(maxQty: Int): Seq[Header] = readModifiers(fileName, maxQty).collect { case h: Header => h }

  val modifierSerializers: Map[ModifierTypeId, ScorexSerializer[_ <: ErgoPersistentModifier]] =
    Map(Header.TypeId -> HeaderSerializer,
      BlockTransactions.TypeId -> BlockTransactionsSerializer,
      ADProofs.TypeId -> ADProofSerializer)

  def readModifiers(fileName: String, threshold: Int): Vector[ErgoPersistentModifier] = {
    var counter = 0
    var headers = 0
    val is = getUrlInputStream(fileName)
    val result = Stream
      .continually{
        counter += 1
        val mod = read(is)
        if (mod.exists(_.modifierTypeId == Header.TypeId)) headers += 1
        mod
      }
      .takeWhile(m => (headers <= threshold) && m.isDefined)
      .flatten
      .toVector
    result
  }

  private def getUrlInputStream(url: String,
                                connectTimeout: Int = 5000,
                                readTimeout: Int = 5000,
                                requestMethod: String = "GET") = {
    val u = new URL(url)
    val conn = u.openConnection.asInstanceOf[HttpsURLConnection]
    conn.setConnectTimeout(connectTimeout)
    conn.setReadTimeout(readTimeout)
    conn.setRequestMethod(requestMethod)
    conn.connect()
    conn.getInputStream
  }

  def read(implicit fis: InputStream): Option[ErgoPersistentModifier] = for {
    typeId <- readModId
    length <- readLength
    bytes <- readBytes(length)
    mod <- modifierSerializers(typeId).parseBytesTry(bytes).toOption
  } yield mod

  private def readModId(implicit fis: InputStream): Option[ModifierTypeId] = {
    val int = fis.read()
    if (int == -1) None else Some(ModifierTypeId @@ int.toByte)
  }

  private def readLength(implicit fis: InputStream): Option[Int] =
    Some(Stream.continually(fis.read().toByte).take(4).toArray).map(Ints.fromByteArray)

  private def readBytes(length: Int)(implicit fis: InputStream): Option[Array[Byte]] =
    Some(Stream.continually(fis.read().toByte).take(length).toArray)

}