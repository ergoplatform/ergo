package org.ergoplatform.nodeView.wallet

import org.ergoplatform._
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, ExtendedSecretKey}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.util.encode.Base16
import scala.util.Try

/**
  * Additional types and functions used in ErgoWalletService
  */
object ErgoWalletServiceUtils {
  /**
   * Result of "deriveNextKey" operation
   */
  case class DeriveNextKeyResult(result: Try[(DerivationPath, P2PKAddress, ExtendedSecretKey)])

  /**
   * Result of "signMessage" operation
   *
   * @param address       - address the message was signed for
   * @param signedMessage - bytes actually signed, the message wrapped as
   *                      [[org.ergoplatform.wallet.crypto.MessageSigning]] prescribes
   * @param proof         - the signature
   */
  case class SignedMessage(address: P2PKAddress, signedMessage: Array[Byte], proof: Array[Byte])

  // A helper which is deserializing Base16-encoded boxes to ErgoBox instances
  def stringsToBoxes(strings: Seq[String]): Seq[ErgoBox] =
    strings.map(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry)).map(_.get)

}

