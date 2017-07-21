package org.ergoplatform.modifiers

import scorex.core.NodeViewModifier

trait ErgoModifier extends NodeViewModifier {

  //TODO do we need version field for all modifiers?
  //val version: Version

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ErgoModifier => that.id sameElements id
    case _ => false
  }
}
