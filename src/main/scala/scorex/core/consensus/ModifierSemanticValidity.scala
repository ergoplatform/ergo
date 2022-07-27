package scorex.core.consensus

/**
  * Outcome of modifier semantic validation
  */
sealed trait ModifierSemanticValidity {
  val code: Byte
}

object ModifierSemanticValidity {

  case object Absent extends ModifierSemanticValidity {
    override val code: Byte = 0
  }

  case object Unknown extends ModifierSemanticValidity {
    override val code: Byte = 1
  }

  case object Valid extends ModifierSemanticValidity {
    override val code: Byte = 2
  }

  case object Invalid extends ModifierSemanticValidity {
    override val code: Byte = 3
  }

}
