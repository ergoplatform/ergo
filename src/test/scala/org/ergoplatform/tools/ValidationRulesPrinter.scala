package org.ergoplatform.tools

import org.ergoplatform.settings.ValidationRules
import scorex.util.ScorexLogging

object ValidationRulesPrinter extends App with ScorexLogging {


  val rules = ValidationRules.rulesSpec

  println("\\textbf{Transaction validation:}\n\n")
  println("\\begin{tabular}{ |p{1.0cm}||p{7.5cm}|p{1.0cm}|p{2.5cm}| }")
  println("    \\hline\nId & Validation rule & Active & Modifiers \\\\\n\\hline")
  rules.toSeq.sortBy(_._1).foreach { r =>

    val rule = r._2.error("").errors.head.message.trim
    val activated = r._2.isActive
    val modifiers = r._2.affectedClasses.map(_.getSimpleName).mkString(", ")

    if (r._1 == 200) {
      println("\\end{tabular}")
      println("\n\n\\textbf{Header validation:}\n\n")
      println("\\begin{tabular}{ |p{1.0cm}||p{7.5cm}|p{1.0cm}|p{2.5cm}| }")
      println("    \\hline\nId & Validation rule & Active & Modifiers \\\\\n\\hline")
    } else if (r._1 == 300) {
      println("\\end{tabular}")
      println("\n\n\\textbf{Block sections validation:}\n\n")
      println("\\begin{tabular}{ |p{1.0cm}||p{7.5cm}|p{1.0cm}|p{2.5cm}| }")
      println("    \\hline\nId & Validation rule & Active & Modifiers \\\\\n\\hline")
    } else if (r._1 == 400) {
      println("\\end{tabular}")
      println("\n\n\\textbf{Block application to state validation:}\n\n")
      println("\\begin{tabular}{ |p{1.0cm}||p{7.5cm}|p{1.0cm}|p{2.5cm}| }")
      println("    \\hline\nId & Validation rule & Active & Modifiers \\\\\n\\hline")
    }

    if (r._2.error("").isFatal) {
      // we only mention fatal errors here

      println(s"    ${r._1} & $rule & $activated & $modifiers \\\\")
      println("    \\hline")
    }
  }
  println("\\end{tabular}")


}
