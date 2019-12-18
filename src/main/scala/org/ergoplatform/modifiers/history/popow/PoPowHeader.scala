package org.ergoplatform.modifiers.history.popow

import io.circe.{Encoder, Json}
import org.ergoplatform.modifiers.history.{Extension, Header, HeaderSerializer}
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

/**
  * Header with unpacked interlinks.
  */
case class PoPowHeader(header: Header, interlinks: Seq[ModifierId])
  extends BytesSerializable {

  override type M = PoPowHeader

  override def serializer: ScorexSerializer[M] = PoPowHeaderSerializer

  def id: ModifierId = header.id

  def height: Int = header.height
}

object PoPowHeader {
  import Header.jsonEncoder

/*
  implicit val jsonEncoder: Encoder[Header] = { p: PoPowHeader =>
    Map(
      "header" -> p.header,
      "interlinks" -> ???
    )
  }*/
}

object Unpacker extends App {

  import io.circe._, io.circe.parser._

  val extS =
    """
      |{
      |  "headerId": "fff3ae6fe0250115cc867b05148b69b10e1e94e56ad8c5d067634eab6dcd8402",
      |  "fields": [
      |    [
      |      "0100",
      |      "01b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b"
      |    ],
      |    [
      |      "0101",
      |      "05557fd0590616b4f6e51eaf54436d61e5585eebfc5a9e860861fc0876064bd3d9"
      |    ],
      |    [
      |      "0106",
      |      "0251ea41ca448e8d3b05f0198bdcda0463f20add5e1e942d4e05c12284310b60ae"
      |    ],
      |    [
      |      "0108",
      |      "01df4149e69849808547d6a1a95edd5f5f026c0a3930eb20b0b221bc240b483fac"
      |    ],
      |    [
      |      "0109",
      |      "01308425a9fec09f07cb84db2e9cb5472f6ad2a5693c478e9863eb6802cdac1e36"
      |    ],
      |    [
      |      "010a",
      |      "01161e4a1a2cf1f2a52e5fd944f15632a8421f5e497165a0d6a846abc15a8c9fb5"
      |    ],
      |    [
      |      "010b",
      |      "04f29044a63ede7a248d5aa36613a0c8f24ea09ab1b8c76b4fe4ea78be135fe418"
      |    ],
      |    [
      |      "010f",
      |      "0251784cfb90e7f0e51f7b80bdb550f8cede3131fac2f9fb2589e12ada87664efc"
      |    ],
      |    [
      |      "0111",
      |      "0272fb52797c86698d7648568e08fd4705699f325a4df61f5378198586657faa6f"
      |    ],
      |    [
      |      "0113",
      |      "015b30675ab847648b658b13fe45d1541376861df381d57018594d7e2e434223ae"
      |    ]
      |  ]
      |}
    """.stripMargin
  val ext = Extension.jsonDecoder.decodeJson(parse(extS).toOption.get).toOption.get
  println(PoPowAlgos.unpackInterlinks(ext.fields).get.apply(6))



}

object PoPowHeaderSerializer extends ScorexSerializer[PoPowHeader] {

  override def serialize(obj: PoPowHeader, w: Writer): Unit = {
    val headerBytes = obj.header.bytes
    w.putUInt(headerBytes.length)
    w.putBytes(headerBytes)
    w.putUInt(obj.interlinks.size)
    obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
  }

  override def parse(r: Reader): PoPowHeader = {
    val headerSize = r.getUInt().toIntExact
    val header = HeaderSerializer.parseBytes(r.getBytes(headerSize))
    val linksQty = r.getUInt().toIntExact
    val interlinks = (0 until linksQty).map(_ => bytesToId(r.getBytes(32)))
    PoPowHeader(header, interlinks)
  }

}
