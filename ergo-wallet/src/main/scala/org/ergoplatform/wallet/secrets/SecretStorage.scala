package org.ergoplatform.wallet.secrets

import java.io.File
import scala.util.Try
import org.ergoplatform.wallet.interface4j.SecretString

/**
  * Secret storage trait.
  * All keys dedicated to a wallet are being derived from
  * a single seed which is stored in a file system in encrypted form.
  * Using SecretString for critical parts to be erased after use.
  */
trait SecretStorage {

  /**
    * Path to the secret file containing encrypted seed
    * and all the required cipher parameters to decrypt
    * the seed providing the correct password.
    */
  val secretFile: File

  /**
    * Tells if `secretsIndices` were locked and destroyed.
    */
  def isLocked: Boolean

  /**
    * Returns the `secretsIndices` if already unlocked, or nothing.
    */
  def secret: Option[ExtendedSecretKey]

  /**
    * Makes secrets with `secretsIndices` available through `secrets` call.
    * @param pass - SecretString password string to be erased after use.
    */
  def unlock(pass: SecretString): Try[Unit]

  /**
    * Destroys all loaded secrets.
    */
  def lock(): Unit

  /**
    * Checks the seed can be decrypted, provided mnemonic with optional mnemonic password.
    * @param mnemonic - SecretString mnemonic string to be erased after use.
    * @param mnemonicPassOpt - optional SecretString mnemonic password to be erased after use.
    */
  def checkSeed(mnemonic: SecretString, mnemonicPassOpt: Option[SecretString]): Boolean

}
