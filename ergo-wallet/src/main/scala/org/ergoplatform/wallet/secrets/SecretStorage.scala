package org.ergoplatform.wallet.secrets

import java.io.File
import scala.util.Try

trait SecretStorage {

  val secretFile: File

  def isLocked: Boolean

  def secret: Option[ExtendedSecretKey]

  def unlock(pass: String): Try[Unit]

  def lock(): Unit

  def checkSeed(mnemonic: String, mnemonicPassOpt: Option[String]): Boolean

}
