package org.ergoplatform.api.services

import scorex.core.VersionTag

import scala.concurrent.Future

trait StateService {

  def getVersion: Future[VersionTag]

  def getType: Future[String]
}
