package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}
import sigmastate.SType
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.serialization.ValueSerializer
import scorex.util.Extensions._


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
      val valueBytes = ValueSerializer.serialize(e.value)
      w.putUInt(valueBytes.length)
      w.putBytes(valueBytes)
    case s: ContainsScanningPredicate =>
      w.put(ContainsPrefix)
      w.put(s.regId.number)
      val valueBytes = ValueSerializer.serialize(s.value)
      w.putUInt(valueBytes.length)
      w.putBytes(valueBytes)
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
    //helper subfunctions
    def parseArgs(r: Reader): Array[ScanningPredicate] = {
      val argsCount = r.getInt()
      val args = new Array[ScanningPredicate](argsCount)
      (0 until argsCount).foreach(idx => args(idx) = parse(r))
      args
    }

    def parseRegisterAndBytes(r: Reader): (RegisterId, EvaluatedValue[_ <: SType]) = {
      val reg = ErgoBox.registerByIndex(r.getByte())
      val len  = r.getUInt().toIntExact
      val bs = r.getBytes(len)
      val vs = ValueSerializer.deserialize(bs)
      reg -> vs.asInstanceOf[EvaluatedValue[SType]]
    }

    val prefix = r.getByte()
    prefix match {
      case b: Byte if b == EqualsPrefix =>
        val (reg, bs) = parseRegisterAndBytes(r)
        EqualsScanningPredicate(reg, bs)
      case b: Byte if b == ContainsPrefix =>
        val (reg, bs) = parseRegisterAndBytes(r)
        ContainsScanningPredicate(reg, bs)
      case b: Byte if b == ContainsAssetPrefix =>
        val bs = r.getBytes(32)
        ContainsAssetPredicate(Digest32 @@ bs)
      case b: Byte if b == AndPrefix =>
        AndScanningPredicate(parseArgs(r) :_*)
      case b: Byte if b == OrPrefix =>
        OrScanningPredicate(parseArgs(r) :_*)
    }
  }
}
