package org.ergoplatform.tools

import org.ergoplatform.settings.ValidationRules
import scorex.util.ScorexLogging

object ValidationRulesPrinter extends App with ScorexLogging {



  val rules = ValidationRules.rulesSpec


  println("\\begin{tabular}{ |p{1.0cm}||p{7.5cm}|p{1.0cm}|p{2.5cm}| }")
  println("\\hline\nId & Validation rule & Active & Modifiers \\\\\n\\hline")
  rules.toSeq.sortBy(_._1).foreach { r =>

    val rule = r._2._1("").errors.head.message
    val activated = r._2._2
    val modifiers = r._2._3.map(_.getSimpleName).mkString(", ")


    if (r._2._1("").isFatal) {
      // we only mention fatal errors here

      println(s"${r._1} & $rule & $activated & $modifiers \\\\")
      println("\\hline")
    }
  }
  println("\\end{tabular}")


}
