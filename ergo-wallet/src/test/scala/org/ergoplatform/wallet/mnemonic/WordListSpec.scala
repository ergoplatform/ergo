package org.ergoplatform.wallet.mnemonic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WordListSpec extends AnyFlatSpec with Matchers {

  behavior of "WordList and Mnemonic"

  it should "load all available wordlists and have 2048 words each" in {
    WordList.AvailableLanguages.foreach { lang =>
      val wl = WordList.load(lang).get
      wl.words.length shouldBe 2048
      wl.words.forall(_.nonEmpty) shouldBe true
    }
  }

  it should "use ideographic space for Japanese and space for others" in {
    val ja = WordList.load("japanese").get
    ja.delimiter shouldBe "\u3000"

    WordList.AvailableLanguages.filterNot(_ == "japanese").foreach { lang =>
      val wl = WordList.load(lang).get
      wl.delimiter shouldBe " "
    }
  }

  it should "generate mnemonic for each language with fixed entropy" in {
    val entropy = Array.fill[Byte](20)(1) // 160 bits entropy
    WordList.AvailableLanguages.foreach { lang =>
      val m = new Mnemonic(lang, 160)
      val phrase = m.toMnemonic(entropy).get
      phrase.getData().nonEmpty shouldBe true
    }
  }
}