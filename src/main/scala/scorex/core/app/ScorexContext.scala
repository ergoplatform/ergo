package scorex.core.app

import java.net.InetSocketAddress

import scorex.core.network.UPnPGateway
import scorex.core.network.message.MessageSpec
import scorex.core.utils.TimeProvider

case class ScorexContext(messageSpecs: Seq[MessageSpec[_]],
                         upnpGateway: Option[UPnPGateway],
                         timeProvider: TimeProvider,
                         externalNodeAddress: Option[InetSocketAddress])
