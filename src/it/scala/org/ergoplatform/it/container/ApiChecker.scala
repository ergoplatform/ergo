package org.ergoplatform.it.container

case class ApiCheckerConfig(apiAddressToCheck: String, specFilePath: String, paramsFilePath: String)

case class ApiChecker(containerId: String, config: ApiCheckerConfig)
