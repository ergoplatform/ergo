package org.ergoplatform.wallet

import org.ergoplatform.wallet.secrets.DerivationPath
import supertagged.TaggedType

object Constants {
  object ScanId extends TaggedType[Short]
  type ScanId = ScanId.Type

  // part of the protocol, do not change
  val SecretKeyLength = 32

  // part of the protocol, do not change
  val ModifierIdLength = 32

  val Encoding = "UTF-8"

  val BitcoinSeed: Array[Byte] = "Bitcoin seed".getBytes(Encoding)

  // Identifiers for system applications of Ergo node
  // Ids below 9 are reserved. Ids from 11 (inclusive) are for user scans

  // SimplePayments scan identifier
  val PaymentsScanId: ScanId = ScanId @@ 10.toShort

  // Scan which is checking mining rewards
  val MiningScanId: ScanId = ScanId @@ 9.toShort


  /**
    * [See EIP-3 https://github.com/ergoplatform/eips/blob/master/eip-0003.md ]
    *
    * For coin type, we suggest consider "ergo" word in ASCII and calculate coin_type number as
    *
    * 101 + 114 + 103 + 111 = 429
    *
    * Following this idea we should use next scheme
    *
    * m / 44' / 429' / account' / change / address_index
    */
  val CoinType = 429

  /**
    * Pre - EIP3 derivation path
    */
  val oldDerivation: DerivationPath = DerivationPath.fromEncoded("m/1").get

  /**
    * Post - EIP3 derivation path
    */
  val eip3DerivationPath: DerivationPath = DerivationPath.fromEncoded("m/44'/429'/0'/0/0").get

}
