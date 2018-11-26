package org.ergoplatform.settings

import org.ergoplatform.settings.Parameters.{KIncrease, MaxBlockCostIncrease, MaxBlockSizeIncrease, MinValuePerByteIncrease}

object LaunchParameters extends Parameters(height = 0, parametersTable = Map(
  KIncrease -> Parameters.Kdefault,
  MinValuePerByteIncrease -> Parameters.MinValuePerByteDefault,
  MaxBlockSizeIncrease -> 512 * 1024,
  MaxBlockCostIncrease -> 1000000
)) {

  import Parameters._

  def parametersDescription: String = {
    """
      |\begin{tabular}{*{6}{l}}
      |Id & Description & Default & Step & Min & Max \\
      |\hline
    """.stripMargin +
      parametersDescs.map { case (id, desc) =>
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

  //println(parametersDescription)
}
