package org.ergoplatform.nodeView

import java.io.File

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.state.{UtxoState, VersionedInMemoryBoxHolder}
import scorex.core.transaction.state.MinimalState.VersionTag

import scala.util.{Failure, Success, Try}

/*
class WrappedUtxoState(dir: File) extends UtxoState(dir) {
  val versionedBoxHolder: VersionedInMemoryBoxHolder

  override def rollbackTo(version: VersionTag): Try[WrappedUtxoState] =
    super.rollbackTo(version)

  override def applyModifier(mod: ErgoPersistentModifier): Try[WrappedUtxoState] = super.applyModifier(mod) match {
    case Success(us) =>
    case Failure(e) => Failure(e)
  }
}*/


