package scorex.core.transaction.state


case class TooHighCostError(message: String) extends Exception(message)


