package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.ErgoBox
import sigmastate.Values.EvaluatedValue
import sigmastate.{SType, Values}
import sigma.Extensions._

/**
  * Basic interface for box scanning predicate functionality
  *
  * See EIP-0001 for details (https://github.com/ergoplatform/eips/blob/master/eip-0001.md)
  */
sealed trait ScanningPredicate {
  def filter(box: ErgoBox): Boolean
}

/**
  * Scanning predicate to track boxes which contain a register which, in turn, contains certain value
  * (wildcard matching, so register contains e.g. value bytes)
  *
  * @param regId - register identifier
  * @param value - sigma-value used in track
  */
case class ContainsScanningPredicate(regId: ErgoBox.RegisterId,
                                     value: EvaluatedValue[SType]) extends ScanningPredicate {

  override def filter(box: ErgoBox): Boolean = {
    value match {
      case Values.ByteArrayConstant(bytes) =>
        box.get(regId).exists {
          _ match {
            case Values.ByteArrayConstant(arr) => arr.toArray.containsSlice(bytes.toArray)
            case _ => false
          }
        }
      case _ => false
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: ContainsScanningPredicate => other.regId == regId && other.value == value
    case _ => false
  }

  override def toString: String = s"ContainsScanningPredicate($regId, $value)"

  override def hashCode(): Int = regId.hashCode() * 31 + value.hashCode()
}


/**
  * Scanning predicate to track boxes which contain a register which, in turn, contains certain bytes
  * (exact matching, so register contains exactly bytes)
  *
  * @param regId - register identifier
  * @param value - bytes to track
  */
case class EqualsScanningPredicate(regId: ErgoBox.RegisterId, value: EvaluatedValue[SType]) extends ScanningPredicate {
  //todo: try to remove boilerplate below
  override def filter(box: ErgoBox): Boolean = {
    value match {
      case Values.ByteArrayConstant(bytes) =>
        if(box.get(regId).isDefined && box.get(regId).get.tpe.equals(value.tpe)) {
          box.get(regId).exists {
            _ match {
              case Values.ByteArrayConstant(arr) => arr.toArray.sameElements(bytes.toArray)
              case _ => false
            }
          }
        } else {
          false
        }
      case Values.GroupElementConstant(groupElement) =>
        box.get(regId).exists {
          _ match {
            case Values.GroupElementConstant(ge) => groupElement == ge
            case _ => false
          }
        }
      case Values.BooleanConstant(bool) =>
        box.get(regId).exists {
          _ match {
            case Values.BooleanConstant(b) => bool == b
            case _ => false
          }
        }
      case Values.IntConstant(int) =>
        box.get(regId).exists {
          _ match {
            case Values.IntConstant(i) => int == i
            case _ => false
          }
        }
      case Values.LongConstant(long) =>
        box.get(regId).exists {
          _ match {
            case Values.IntConstant(l) => long == l
            case _ => false
          }
        }
      case _ => false
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: EqualsScanningPredicate => other.regId == regId && other.value == value
    case _ => false
  }

  override def hashCode(): Int = regId.hashCode() * 31 + value.hashCode()

  override def toString: String = s"EqualsScanningPredicate($regId, $value)"
}


/**
  * Scanning predicate to track boxes which certain asset.
  *
  * @param assetId - bytes to track
  */
case class ContainsAssetPredicate(assetId: ErgoBox.TokenId) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = {
    box.additionalTokens.exists(_._1 == assetId)
  }

  override def toString: String = s"ContainsAssetPredicate(${assetId.toHex})"
}


/**
  * Scanning predicate to track boxes which satisfy all the sub-predicates at the same time.
  *
  * @param subPredicates - arbitrary number of sub-predicates
  */
case class AndScanningPredicate(subPredicates: ScanningPredicate*) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = subPredicates.forall(p => p.filter(box))
}


/**
  * Scanning predicate to track boxes which satisfy any of the sub-predicates.
  *
  * @param subPredicates - arbitrary number of sub-predicates
  */
case class OrScanningPredicate(subPredicates: ScanningPredicate*) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = subPredicates.exists(p => p.filter(box))
}
