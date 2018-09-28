package org.ergoplatform.it.container

case class ApiCheckerConfig(apiAddress: String, specPath: String, paramsPath: String)

case class ApiChecker(containerId: String, cfg: ApiCheckerConfig)
