package org.ergoplatform.tools

import org.ergoplatform.settings.ValidationRules
import scorex.util.ScorexLogging

object ValidationRulesPrinter extends App with ScorexLogging {

  val rules = ValidationRules.rulesSpec

  println("\\textbf{Transaction validation:}\n\n")
  printHeader()
  rules.toSeq.sortBy(_._1).foreach { r =>

    val rule = r._2.error("").errors.head.message.trim
    val activated = r._2.isActive
    val mayBeDisabled = r._2.mayBeDisabled
    val modifiers = r._2.affectedClasses.map(_.getSimpleName).mkString(", ")

    if (r._1 == 200) {
      println("\\end{tabular}")
      println("\n\n\\textbf{Header validation:}\n\n")
      printHeader()
    } else if (r._1 == 300) {
      println("\\end{tabular}")
      println("\n\n\\textbf{Block sections validation:}\n\n")
      printHeader()
    } else if (r._1 == 400) {
      println("\\end{tabular}")
      println("\n\n\\textbf{Extension validation:}\n\n")
      printHeader()
    } else if (r._1 == 500) {
      println("\\end{tabular}")
      println("\n\n\\textbf{Block application to state validation:}\n\n")
      printHeader()
    }

    if (r._2.error("").isFatal) {
      // we only mention fatal errors here

      println(s"    ${r._1} & $rule & ${boolToLatex(mayBeDisabled)} & ${boolToLatex(activated)} & $modifiers \\\\")
      println("    \\hline")
    }
  }
  println("\\end{tabular}")


  def printHeader(): Unit = {
    println("\\begin{tabular}{ |p{1.0cm}||p{7.5cm}|p{1.0cm}|p{1.0cm}|p{2.5cm}| }")
    println("    \\hline\nId & Validation rule & Soft-forkable & Active & Modifiers \\\\\n\\hline")
  }

  def boolToLatex(v: Boolean): String = if (v) "\\cmark" else "\\xmark"

}
