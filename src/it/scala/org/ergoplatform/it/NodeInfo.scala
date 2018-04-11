package org.ergoplatform.it

case class NodeInfo(
  hostRestApiPort: Int,
  hostNetworkPort: Int,
  containerNetworkPort: Int,
  apiIpAddress: String,
  networkIpAddress: String,
  containerId: String
)
