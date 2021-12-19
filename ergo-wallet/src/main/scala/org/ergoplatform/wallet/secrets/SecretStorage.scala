package org.ergoplatform.wallet.secrets

import java.io.File
import scala.util.Try
import org.ergoplatform.wallet.interface4j.SecretString

/**
  * Secret storage trait.
  * Using SecretString for critical parts to be erased after use
  */
trait SecretStorage {

  val secretFile: File

  def isLocked: Boolean

  def secret: Option[ExtendedSecretKey]

  def unlock(pass: SecretString): Try[Unit]

  def lock(): Unit

  /**
    * @param mnemonic - SecretString to be erased after use
    */
  def checkSeed(mnemonic: SecretString, mnemonicPassOpt: Option[SecretString]): Boolean

}
