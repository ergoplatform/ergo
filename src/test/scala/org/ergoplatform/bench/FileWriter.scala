package org.ergoplatform.bench

import java.io.FileWriter

import scala.io.Source
import scala.util.Try


trait WritableData {
  def toDataLine: String
}

object FileWriter {

  val linesLimit = 100

  def writeToFile(fileName: String, data: WritableData): Unit = {
    val lines: List[String] = Try(Source.fromFile(fileName).getLines().toList).getOrElse(List.empty)
    val resultToSave = if (lines.length < linesLimit) { lines :+ data.toDataLine }
    else { lines.takeRight(linesLimit - 1) :+ data.toDataLine }
    val file = new java.io.File(fileName)
    val writer = new FileWriter(file)
    resultToSave.foreach { l =>
      writer.write(l)
      writer.write('\n')
    }
    writer.flush()
    writer.close()
  }

}
