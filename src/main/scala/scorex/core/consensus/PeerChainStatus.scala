package scorex.core.consensus


sealed trait PeerChainStatus

case object Equal extends PeerChainStatus

case object Younger extends PeerChainStatus

case object Fork extends PeerChainStatus

case object Older extends PeerChainStatus

case object Nonsense extends PeerChainStatus

case object Unknown extends PeerChainStatus
