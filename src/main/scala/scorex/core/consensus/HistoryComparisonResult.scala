package scorex.core.consensus


sealed trait HistoryComparisonResult

case object Equal extends HistoryComparisonResult

case object Younger extends HistoryComparisonResult

case object Fork extends HistoryComparisonResult

case object Older extends HistoryComparisonResult

case object Nonsense extends HistoryComparisonResult

case object Unknown extends HistoryComparisonResult
