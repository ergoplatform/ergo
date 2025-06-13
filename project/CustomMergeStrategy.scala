import java.io.{File, FileOutputStream}

import sbt.IO
import sbtassembly.MergeStrategy
import sbtassembly.MergeStrategy.createMergeTarget

object CustomMergeStrategy {

  /** Merge strategy suitable for configs merging. */
  val concatReversed: MergeStrategy = new MergeStrategy {
    val name = "concatReversed"
    def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] = {
      val file = createMergeTarget(tempDir, path)
      val out = new FileOutputStream(file)
      try {
        files.reverse foreach {f =>
          IO.transfer(f, out)
          if (!IO.read(f).endsWith(IO.Newline)) out.write(IO.Newline.getBytes(IO.defaultCharset))
        }
        Right(Seq(file -> path))
      } finally {
        out.close()
      }
    }
  }

}
