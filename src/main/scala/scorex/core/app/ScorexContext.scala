package scorex.core.app

import java.net.InetSocketAddress

import scorex.core.network.UPnPGateway
import org.ergoplatform.network.message.MessageSpec

case class ScorexContext(messageSpecs: Seq[MessageSpec[_]],
                         upnpGateway: Option[UPnPGateway],
                         externalNodeAddress: Option[InetSocketAddress])
