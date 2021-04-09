package org.ergoplatform.utils

import java.io.{OutputStreamWriter, Writer, ByteArrayOutputStream}

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.{DataInput, ErgoBox, Input}
import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Header, BlockTransactions, ADProofs, Extension}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.state.UtxoState.{createUtxoStateReporter, appendFullBlockReporter}
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.utils.metrics.{CsvFileCollector, CsvCollector, Reporter, measureCostedOp, MeasuredObject, ApplyTransactionsReporter, measureOp}
import scalan.RType
import scalan.RType._
import scalan.util.FileUtil
import sigmastate.eval._
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.{SBoolean, BoolToSigmaProp}
import sigmastate.Values.{ConstantPlaceholder, TrueLeaf, ErgoTree}
import sigmastate.eval.CostingSigmaDslBuilder
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.{ContextExtension, CostedProverResult}
import sigmastate.utils.Helpers

import scala.collection.mutable
import scala.util.Success

class MetricsSpec extends ErgoPropertyTest {
  def Coll[T](items: T*)(implicit cT: RType[T]) = CostingSigmaDslBuilder.Colls.fromItems(items:_*)

  class TestCsvCollector extends CsvCollector {
    val outputs = mutable.HashMap.empty[String, ByteArrayOutputStream]

    override protected def createOutputWriter[D](r: metrics.Reporter[D]): Writer = {
      val baos = outputs.getOrElseUpdate(r.metricName, new ByteArrayOutputStream())
      val w = new OutputStreamWriter(baos)
      writeHeader(r, w)
      w
    }
  }

  def parseCsvText(text: String): Seq[Seq[String]] = {
    text
      .split(System.lineSeparator).toSeq
      .map(line => line.split(';').dropRight(1): Seq[String])
  }

  def checkOutput[D](c: TestCsvCollector, r: Reporter[D], expOut: Seq[Seq[String]]) = {
    val m1 = c.outputs(r.metricName)
    val out = parseCsvText(m1.toString())
    if (expOut.isEmpty)
      SigmaPPrint.pprintln(out)
    out shouldBe expOut
  }

  def performMeasuredOps(block: ErgoFullBlock) = {
    measureOp(block, appendFullBlockReporter) {
      Thread.sleep(5)
      Success(())
    }
    measureCostedOp(block, ApplyTransactionsReporter) {
      Thread.sleep(10)
      Success(10000L)
    }
    measureOp(block, createUtxoStateReporter) {
      Thread.sleep(15)
      Success(())
    }
  }

  lazy val block = ErgoFullBlock(
    Header(
      1.toByte,
      ModifierId @@ "0000000000000000000000000000000000000000000000000000000000000000",
      Digest32 @@ ErgoAlgos.decodeUnsafe("f1ad458e878e076eac388794a8b23b8ed0bdae3f1c510fabbff49719eab49f27"),
      ADDigest @@ ErgoAlgos.decodeUnsafe("9614118b3ed74d8b1995ae3135e287d1e6f964b1f37fd12571559654c4a9f79406"),
      Digest32 @@ ErgoAlgos.decodeUnsafe("3733814a0546aba6fdcca8b7e821a547ae1a8391101de34bc7641841071c5886"),
      1617891975926L,
      16842752L,
      1,
      Digest32 @@ ErgoAlgos.decodeUnsafe("7d5dfdbdd3feaed06e8dfd3ae0357b4abbdde47a7caa1e199070626de965940f"),
      AutolykosSolution(
        Helpers.decodeECPoint("038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c"),
        Helpers.decodeECPoint("02899c562ae9646cea842daef3747cf5b6b81ee18fa38cbf287c057cc98d5a8830"),
        ErgoAlgos.decodeUnsafe("0000000000000000"),
        BigInt("1745d1745d1745d1745d1745d1745d17283e712c3e7af74b3ffbda0ccd1c347a", 16)
      ),
      ErgoAlgos.decodeUnsafe("000000"),
      None
    ),
    BlockTransactions(
      ModifierId @@ "febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d",
      1.toByte,
      List(
        ErgoTransaction(
          Vector(
            Input(
              ADKey @@ ErgoAlgos.decodeUnsafe("a61ca603a2bf38daed45707bcf2cdfe0fb8b9fc7ec7b10e6aa40776bcbed045b"),
              CostedProverResult(
                ErgoAlgos.decodeUnsafe(
                  "fe27ce2d2259e1251a8fd5c58204678fcd152dd0bc31d14347dceecc2db064417bd47c6b8a88921c1b10e5300fb99a10ca943f8005ecb141"
                ),
                ContextExtension(Map()),
                24094L
              )
            )
          ),
          Vector(
            DataInput(ADKey @@ ErgoAlgos.decodeUnsafe("3bfaf76c824df668822dfce71abaf688d0281f91c3ac2a271f92fa28c3efaac7")),
            DataInput(ADKey @@ ErgoAlgos.decodeUnsafe("a61ca603a2bf38daed45707bcf2cdfe0fb8b9fc7ec7b10e6aa40776bcbed045b")),
            DataInput(ADKey @@ ErgoAlgos.decodeUnsafe("2347cf01ff093b79e8e66643a88a4a1525cc97558efdf4f88fd5ad7a3967e5db")),
            DataInput(ADKey @@ ErgoAlgos.decodeUnsafe("51509e78384e63b392865651a168065a3cb6cf2563cdc452525662e493a88bbf"))
          ),
          Vector(
            new ErgoBox(
              67498525440L,
              new ErgoTree(16.toByte, Vector(TrueLeaf), Right(BoolToSigmaProp(ConstantPlaceholder(0, SBoolean)))),
              Coll[(TokenId, Long)]((Digest32 @@ (ErgoAlgos.decodeUnsafe("a61ca603a2bf38daed45707bcf2cdfe0fb8b9fc7ec7b10e6aa40776bcbed045b")), 1078829822L)),
              Map(),
              ModifierId @@ ("0000000000000000000000000000000000000000000000000000000000000000"),
              0.toShort,
              0
            ),
            new ErgoBox(
              1474560L,
              new ErgoTree(16.toByte, Vector(TrueLeaf), Right(BoolToSigmaProp(ConstantPlaceholder(0, SBoolean)))),
              Coll((Digest32 @@ (ErgoAlgos.decodeUnsafe("a61ca603a2bf38daed45707bcf2cdfe0fb8b9fc7ec7b10e6aa40776bcbed045b")), 832012790L)),
              Map(),
              ModifierId @@ ("0000000000000000000000000000000000000000000000000000000000000000"),
              0.toShort,
              0
            )
          ),
          None
        ),
        ErgoTransaction(
          Vector(
            Input(
              ADKey @@ ErgoAlgos.decodeUnsafe("ed3909d1a42c1faf80c2f62863c89a5c67c62d7a54c482542304f23f0affb08a"),
              CostedProverResult(ErgoAlgos.decodeUnsafe(""), ContextExtension(Map()), 14514L)
            ),
            Input(
              ADKey @@ ErgoAlgos.decodeUnsafe("b80ed15b258ac4a1275048716bd16d07caa2f03027cad39553e2099b2f402d5b"),
              CostedProverResult(ErgoAlgos.decodeUnsafe(""), ContextExtension(Map()), 14728L)
            )
          ),
          Vector(),
          Vector(
            new ErgoBox(
              1474560L,
              new ErgoTree(16.toByte, Vector(TrueLeaf), Right(BoolToSigmaProp(ConstantPlaceholder(0, SBoolean)))),
              Coll((Digest32 @@ (ErgoAlgos.decodeUnsafe("a61ca603a2bf38daed45707bcf2cdfe0fb8b9fc7ec7b10e6aa40776bcbed045b")), 707276152L)),
              Map(),
              ModifierId @@ ("0000000000000000000000000000000000000000000000000000000000000000"),
              0.toShort,
              0
            ),
            new ErgoBox(
              32557054542L,
              new ErgoTree(16.toByte, Vector(TrueLeaf), Right(BoolToSigmaProp(ConstantPlaceholder(0, SBoolean)))),
              Coll(),
              Map(),
              ModifierId @@ ("0000000000000000000000000000000000000000000000000000000000000000"),
              0.toShort,
              0
            ),
            new ErgoBox(
              34941470898L,
              new ErgoTree(16.toByte, Vector(TrueLeaf), Right(BoolToSigmaProp(ConstantPlaceholder(0, SBoolean)))),
              Coll((Digest32 @@ (ErgoAlgos.decodeUnsafe("a61ca603a2bf38daed45707bcf2cdfe0fb8b9fc7ec7b10e6aa40776bcbed045b")), 124736638L)),
              Map(),
              ModifierId @@ ("0000000000000000000000000000000000000000000000000000000000000000"),
              0.toShort,
              0
            )
          ),
          None
        )
      ),
      None
    ),
    Extension(
      ModifierId @@ "febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d",
      Vector(
        (ErgoAlgos.decodeUnsafe("0005"), ErgoAlgos.decodeUnsafe("00000064")),
        (ErgoAlgos.decodeUnsafe("0001"), ErgoAlgos.decodeUnsafe("001312d0")),
        (ErgoAlgos.decodeUnsafe("0006"), ErgoAlgos.decodeUnsafe("000007d0")),
        (ErgoAlgos.decodeUnsafe("0002"), ErgoAlgos.decodeUnsafe("00000168")),
        (ErgoAlgos.decodeUnsafe("0007"), ErgoAlgos.decodeUnsafe("00000064")),
        (ErgoAlgos.decodeUnsafe("0003"), ErgoAlgos.decodeUnsafe("00080000")),
        (ErgoAlgos.decodeUnsafe("007b"), ErgoAlgos.decodeUnsafe("00000001")),
        (ErgoAlgos.decodeUnsafe("0008"), ErgoAlgos.decodeUnsafe("00000064")),
        (ErgoAlgos.decodeUnsafe("0004"), ErgoAlgos.decodeUnsafe("000f4240")),
        (ErgoAlgos.decodeUnsafe("007c"), ErgoAlgos.decodeUnsafe("0000"))
      ),
      None
    ),
    Some(
      ADProofs(
        ModifierId @@ "febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d",
        SerializedAdProof @@ ErgoAlgos.decodeUnsafe(
          "0200000000000000000000000000000000000000000000000000000000000000002347cf01ff093b79e8e66643a88a4a1525cc97558efdf4f88fd5ad7a3967e5db00000000023bfaf76c824df668822dfce71abaf688d0281f91c3ac2a271f92fa28c3efaac70000012a80d6d0c7cfdad807100e040004c094400580809cde91e7b0010580acc7f03704be944004808948058080c7b7e4992c0580b4c4c32104fe884804c0fd4f0580bcc1960b04befd4f05000400ea03d192c1b2a5730000958fa373019a73029c73037e997304a305958fa373059a73069c73077e997308a305958fa373099c730a7e99730ba305730cd193c2a7c2b2a5730d00d50408000000010e6f98040483030808cd038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c08cd031ee1ab3b729f21e0dcee05642a63745286354d8e511e6376838e235a28078c0108cd0248502b73f35bb2b77eb5ad16f80f55beff178ccd104488edd2d8b69c192c109d000000000000000000000000000000000000000000000000000000000000000000000251509e78384e63b392865651a168065a3cb6cf2563cdc452525662e493a88bbf000001d48094ebdc0310010100d173000000050e41274368616f7320726569676e73273a20776861742074686520706170657273207361792061626f757420746865206e6f2d6465616c2042726578697420766f74650e49e4b9a0e8bf91e5b9b3e79a84e4b8a4e4bc9ae697b6e997b47ce8bf99e9878ce69c89e4bbbde4b9a0e8bf91e5b9b3e4b8a4e4bc9ae697a5e58e86efbc8ce8afb7e69fa5e694b6efbc810e8d01d0a2d090d0a1d0a120d181d0bed0bed0b1d189d0b8d0bb20d0bed0b120d0bed0b1d0bdd0b0d180d183d0b6d0b5d0bdd0b8d0b820d0bdd0b5d181d0bad0bed0bbd18cd0bad0b8d18520d0bcd0b0d0b9d0bdd0b8d0bdd0b3d0bed0b2d18bd18520d184d0b5d180d0bc20d0bdd0b020d181d182d0bed0bbd0b8d187d0bdd18bd18520d180d18bd0bdd0bad0b0d1850e40303030303030303030303030303030303030313339613365363162643537323138323762353161353330396138626665636130623863346235633036303933310e4230786566316435383464373765373465336335303964653632356463313738393362323262373364303430623564353330326262663833323036356639323864303300000000000000000000000000000000000000000000000000000000000000000002ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000011180bac28bc7e3f6a501101004020e36100204cf0f08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a7017300730110010204020404040004c0fd4f05808c82f5f6030580b8c9e5ae040580f882ad16040204c0944004c0f407040004000580f882ad16d19683030191a38cc7a7019683020193c2b2a57300007473017302830108cdeeac93a38cc7b2a573030001978302019683040193b1a5730493c2a7c2b2a573050093958fa3730673079973089c73097e9a730a9d99a3730b730c0599c1a7c1b2a5730d00938cc7b2a5730e0001a390c1a7730f00000000000000000000000000000000000000000000000000000000000000000000000000000412601103ce808a9000420c00040008032030080b1fc28020000cb2000424200000030010000820000000c02504000000000000000000000000000000000000"
        ),
        None
      )
    )
  )

  val appendFullBlockRows = Seq[Seq[String]](
    Array("id", "cost"),
    Array("febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d", "-1")
  )
  val applyTransactionsRows = Seq[Seq[String]](
    Array("blockId", "tx_num", "cost"),
    Array("febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d", "2", "10000")
  )
  val createUtxoStateRows = Seq[Seq[String]](
    Array("id", "cost"),
    Array("febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d", "-1")
  )

  property("CsvCollector: expected csv rows") {
    val collector = new TestCsvCollector

    metrics.executeWithCollector(collector) {
      performMeasuredOps(block)
    }

    collector.flush()

    checkOutput(collector, appendFullBlockReporter, appendFullBlockRows)

    checkOutput(collector, ApplyTransactionsReporter, applyTransactionsRows)

    checkOutput(collector, createUtxoStateReporter, createUtxoStateRows)
      
    collector.close()
  }

  property("CsvFileCollector should create files") {
    val metricsDir = settings.directory + "/metrics"

    if (FileUtil.file(metricsDir).exists())
      FileUtil.delete(FileUtil.file(metricsDir))

    try {
      val c = new CsvFileCollector(metricsDir)
      metrics.executeWithCollector(c) {
        performMeasuredOps(block)
      }
      c.flush()

      val reporters = Seq(
        (appendFullBlockReporter, appendFullBlockRows),
        (ApplyTransactionsReporter, applyTransactionsRows),
        (createUtxoStateReporter, createUtxoStateRows)
      )

      reporters.foreach { case (r, expRows) =>
        val f = c.getMetricFile(r)
        f.exists() shouldBe true
        val text = FileUtil.read(f)
        parseCsvText(text) shouldBe expRows
      }

    } finally {
      // cleanup metrics dir
      FileUtil.delete(FileUtil.file(metricsDir))
    }
  }

}
