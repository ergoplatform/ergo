package org.ergoplatform.wallet.mnemonic

import java.text.Normalizer.Form.NFKD
import java.text.Normalizer.normalize

import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import scodec.bits.BitVector

import scala.util.{Failure, Try}

/**
  * BIP39 mnemonic sentence (see: https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
  * @param languageId - language identifier to be used in sentence
  * @param strength - number of bits in the seed
  */
final class Mnemonic(languageId: String, strength: Int) {

  import Mnemonic._

  /**
    * Generates new mnemonic phrase from system randomness.
    */
  def generate: Try[String] = {
    if (!AllowedStrengths.contains(strength))
      Failure(new Error(s"Strength should be one of $AllowedStrengths, but it is $strength."))
    else toMnemonic(scorex.utils.Random.randomBytes(strength / 8))
  }

  /**
    * Generates new mnemonic phrase from a given entropy.
    */
  def toMnemonic(entropy: Array[Byte]): Try[String] = {
    if (!AllowedEntropyLengths.contains(entropy.length))
      Failure(new Error(s"Entropy length should be one of $AllowedEntropyLengths, but it is ${entropy.length}."))
    else {
      val checksum = BitVector(scorex.crypto.hash.Sha256.hash(entropy))
      val entropyWithChecksum = BitVector(entropy) ++ checksum.take(entropy.length / 4)

      WordList.load(languageId).map { wordList =>
        entropyWithChecksum
          .grouped(BitsGroupSize)
          .map { wordIndex =>
            wordList.words(wordIndex.toInt(signed = false))
          }
          .mkString(wordList.delimiter)
      }
    }
  }

}

object Mnemonic {

  val MnemonicSentenceSizes: Seq[Int] = Seq(12, 15, 18, 21, 24)
  val AllowedStrengths: Seq[Int] = Seq(128, 160, 192, 224, 256)
  val AllowedEntropyLengths: Seq[Int] = AllowedStrengths.map(_ / 8)
  val BitsGroupSize = 11
  val Pbkdf2Algorithm = "PBKDF2WithHmacSHA512"
  val Pbkdf2Iterations = 2048 // number of iteration specified in BIP39 standard.
  val Pbkdf2KeyLength = 512

  /**
    * Converts mnemonic phrase to seed it was derived from.
    */
  def toSeed(mnemonic: String, passOpt: Option[String] = None): Array[Byte] = {
    val normalizedMnemonic = normalize(mnemonic.toCharArray, NFKD).toCharArray
    val normalizedSeed = normalize(s"mnemonic${passOpt.getOrElse("")}", NFKD)

    val spec = new PBEKeySpec(
      normalizedMnemonic,
      normalizedSeed.getBytes,
      Pbkdf2Iterations,
      Pbkdf2KeyLength
    )
    val skf = SecretKeyFactory.getInstance(Pbkdf2Algorithm)
    skf.generateSecret(spec).getEncoded
  }

}
