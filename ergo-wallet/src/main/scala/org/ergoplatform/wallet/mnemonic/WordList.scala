package org.ergoplatform.wallet.mnemonic

import scala.io.{BufferedSource, Codec, Source}
import scala.util.{Failure, Try}

final case class WordList(words: Seq[String], delimiter: String)

object WordList {

  val AvailableLanguages: Seq[String] = Seq(
    "chinese_simplified",
    "chinese_traditional",
    "english",
    "french",
    "italian",
    "japanese",
    "korean",
    "spanish"
  )

  def load(languageId: String): Try[WordList] = languageId match {
    case "japanese" => loadFile(resourceLoader("japanese.txt")).map(WordList(_, "\u3000"))
    case other if AvailableLanguages contains other => loadFile(resourceLoader(s"$other.txt")).map(WordList(_, " "))
    case other => Failure(new IllegalArgumentException(s"Unknown language $other"))
  }

  private def loadFile(loadFile: () => BufferedSource): Try[Seq[String]] =
    Try(loadFile()).map(r => try r.getLines().toList finally r.close())

  private def resourceLoader(fileName: String): () => BufferedSource =
    () => Source.fromResource(s"wordlist/$fileName")(Codec.UTF8)
}
