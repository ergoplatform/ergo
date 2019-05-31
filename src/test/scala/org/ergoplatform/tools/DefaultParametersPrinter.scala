package org.ergoplatform.tools

import org.ergoplatform.settings.LaunchParameters.parametersTable
import org.ergoplatform.settings.Parameters.{maxValues, minValues, parametersDescs, stepsTable}

object DefaultParametersPrinter extends App {
  lazy val parametersDescription: String = {
    """
      |\begin{tabular}{*{6}{l}}
      |Id & Description & Default & Step & Min & Max \\
      |\hline
    """.stripMargin +
      parametersDescs.map { case (id, (_, desc)) =>
        val defaultOpt = parametersTable.get(id)
        val stepOpt = stepsTable.get(id)
        val minValue = minValues.get(id)
        val maxValue = maxValues.get(id)
        s"$id & $desc & ${defaultOpt.getOrElse("-")} & ${stepOpt.getOrElse("-")} & ${minValue.getOrElse("-")} & ${maxValue.getOrElse("-")} \\\\"
      }.mkString("\n") +
      """
        |\end{tabular}
      """.stripMargin
  }

  print(parametersDescription)
}
