package org.ergoplatform.it.container

case class NodeInfo(hostRestApiPort: Int,
                    hostNetworkPort: Int,
                    containerNetworkPort: Int,
                    containerApiPort: Int,
                    apiIpAddress: String,
                    networkIpAddress: String,
                    containerId: String)
