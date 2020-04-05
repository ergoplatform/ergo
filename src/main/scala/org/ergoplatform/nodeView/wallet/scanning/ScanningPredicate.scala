package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.ErgoBox
import scorex.util.encode.Base16
import sigmastate.Values

/**
  * Basic interface for box scanning predicate functionality
  *
  * See EIP-0001 for details (https://github.com/ergoplatform/eips/blob/master/eip-0001.md)
  */
sealed trait ScanningPredicate {
  def filter(box: ErgoBox): Boolean
}

case class ContainsScanningPredicate(regId: ErgoBox.RegisterId, bytes: Array[Byte]) extends ScanningPredicate {

  override def filter(box: ErgoBox): Boolean = {
    box.get(regId).exists {
      _ match {
        case Values.ByteArrayConstant(arr) => arr.toArray.containsSlice(bytes)
        case _ => false
      }
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: ContainsScanningPredicate => other.regId == regId && other.bytes.sameElements(bytes)
    case _ => false
  }

  override def toString: String = s"ContainsScanningPredicate($regId, ${Base16.encode(bytes)})"

  override def hashCode(): Int = regId.hashCode() * 31 + bytes.toSeq.hashCode()
}

case class EqualsScanningPredicate(regId: ErgoBox.RegisterId, bytes: Array[Byte]) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = {
    box.get(regId).exists {
      _ match {
        case Values.ByteArrayConstant(arr) => arr.toArray.sameElements(bytes)
        case _ => false
      }
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: EqualsScanningPredicate => other.regId == regId && other.bytes.sameElements(bytes)
    case _ => false
  }

  override def hashCode(): Int = regId.hashCode() * 31 + bytes.toSeq.hashCode()

  override def toString: String = s"EqualsScanningPredicate($regId, ${Base16.encode(bytes)})"
}

case class ContainsAssetPredicate(assetId: ErgoBox.TokenId) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = {
    box.additionalTokens.exists(_._1.sameElements(assetId))
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: ContainsAssetPredicate => other.assetId.sameElements(assetId)
    case _ => false
  }

  override def hashCode(): Int = assetId.toSeq.hashCode()

  override def toString: String = s"ContainsAssetPredicate(${Base16.encode(assetId)})"
}

case class AndScanningPredicate(subPredicates: ScanningPredicate*) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = subPredicates.forall(p => p.filter(box))
}

case class OrScanningPredicate(subPredicates: ScanningPredicate*) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = subPredicates.exists(p => p.filter(box))
}
