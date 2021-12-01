package org.ergoplatform.wallet.secrets

import java.io.File
import scala.util.Try
import org.ergoplatform.wallet.interface4j.SecretString

trait SecretStorage {

  val secretFile: File

  def isLocked: Boolean

  def secret: Option[ExtendedSecretKey]

  def unlock(pass: SecretString): Try[Unit]

  def lock(): Unit

  def checkSeed(mnemonic: SecretString, mnemonicPassOpt: Option[SecretString]): Boolean

}
