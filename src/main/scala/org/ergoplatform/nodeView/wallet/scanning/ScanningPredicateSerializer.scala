package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.ErgoBox
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}


object ScanningPredicateSerializer extends ScorexSerializer[ScanningPredicate] {

  val EqualsPrefix = 1: Byte
  val ContainsPrefix = 2: Byte
  val ContainsAssetPrefix = 3: Byte
  val AndPrefix = 4: Byte
  val OrPrefix = 5: Byte

  override def serialize(obj: ScanningPredicate, w: Writer): Unit = obj match {
    case e: EqualsScanningPredicate =>
      w.put(EqualsPrefix)
      w.put(e.regId.number)
      w.putInt(e.bytes.length)
      w.putBytes(e.bytes)
    case s: ContainsScanningPredicate =>
      w.put(ContainsPrefix)
      w.put(s.regId.number)
      w.putInt(s.bytes.length)
      w.putBytes(s.bytes)
    case a: ContainsAssetPredicate =>
      w.put(ContainsAssetPrefix)
      w.putBytes(a.assetId)
    case AndScanningPredicate(subPredicates@_*) =>
      w.put(AndPrefix)
      w.putInt(subPredicates.length)
      subPredicates.foreach(sp => serialize(sp, w))
    case OrScanningPredicate(subPredicates@_*) =>
      w.put(OrPrefix)
      w.putInt(subPredicates.length)
      subPredicates.foreach(sp => serialize(sp, w))
  }

  override def parse(r: Reader): ScanningPredicate = {
    val prefix = r.getByte()
    prefix match {
      case b: Byte if b == EqualsPrefix =>
        val reg = ErgoBox.registerByIndex(r.getByte())
        val len = r.getInt()
        val bs = r.getBytes(len)
        EqualsScanningPredicate(reg, bs)
      case b: Byte if b == ContainsPrefix =>
        val reg = ErgoBox.registerByIndex(r.getByte())
        val len = r.getInt()
        val bs = r.getBytes(len)
        ContainsScanningPredicate(reg, bs)
      case b: Byte if b == ContainsAssetPrefix =>
        val bs = r.getBytes(32)
        ContainsAssetPredicate(Digest32 @@ bs)
      case b: Byte if b == AndPrefix =>
        val argsCount = r.getInt()
        AndScanningPredicate((1 to argsCount).map(_ => parse(r)) :_*)
      case b: Byte if b == OrPrefix =>
        val argsCount = r.getInt()
        OrScanningPredicate((1 to argsCount).map(_ => parse(r)) :_*)
    }
  }
}