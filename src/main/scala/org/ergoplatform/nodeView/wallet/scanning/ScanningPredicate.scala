package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.ErgoBox
import sigma.Extensions._
import sigma.ast.{BooleanConstant, ByteArrayConstant, EvaluatedValue, GroupElementConstant, IntConstant, LongConstant, SType}

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
      case ByteArrayConstant(bytes) =>
        box.get(regId).exists {
          _ match {
            case ByteArrayConstant(arr) => arr.toArray.containsSlice(bytes.toArray)
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
      case ByteArrayConstant(bytes) =>
        if(box.get(regId).isDefined && box.get(regId).get.tpe.equals(value.tpe)) {
          box.get(regId).exists {
            _ match {
              case ByteArrayConstant(arr) => arr.toArray.sameElements(bytes.toArray)
              case _ => false
            }
          }
        } else {
          false
        }
      case GroupElementConstant(groupElement) =>
        box.get(regId).exists {
          _ match {
            case GroupElementConstant(ge) => groupElement == ge
            case _ => false
          }
        }
      case BooleanConstant(bool) =>
        box.get(regId).exists {
          _ match {
            case BooleanConstant(b) => bool == b
            case _ => false
          }
        }
      case IntConstant(int) =>
        box.get(regId).exists {
          _ match {
            case IntConstant(i) => int == i
            case _ => false
          }
        }
      case LongConstant(long) =>
        box.get(regId).exists {
          _ match {
            case IntConstant(l) => long == l
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
