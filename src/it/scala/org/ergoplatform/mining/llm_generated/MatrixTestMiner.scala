package org.ergoplatform.mining.llm_generated

import com.google.common.primitives.Ints
import io.circe.Json
import org.ergoplatform.{AutolykosSolution, InputSolutionFound, OrderingSolutionFound}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.settings.Parameters
import scorex.util.encode.Base16

import scala.concurrent.duration._

object MatrixTestMiner {
  def solve(
    work: Json,
    scheme: AutolykosPowScheme,
    secret: BigInt,
    parameters: Parameters,
    inputBlock: Boolean
  ): AutolykosSolution = {
    val cursor = work.hcursor
    val messageHex = cursor.get[String]("msg").fold(throw _, identity)
    val message = Base16.decode(messageHex).get
    val targetJson = cursor.downField("b").focus.getOrElse(
      throw new IllegalArgumentException("Mining work has no target b")
    )
    // The node encodes b as an exact JSON number; accept decimal strings as well.
    val target = targetJson.asNumber.flatMap(_.toBigInt).getOrElse {
      BigInt(targetJson.asString.getOrElse(
        throw new IllegalArgumentException("Mining target b must be an integer")
      ))
    }
    val height = cursor.get[Option[Int]]("h").fold(throw _, identity)
    require(!inputBlock || height.isDefined, "Input-block work must include height h")
    require(target > 0, "Mining target b must be positive")
    // This selects the solver mode only: all header versions >= 2 use Autolykos v2.
    val solverVersion: Byte = if (height.isDefined) 2 else 1
    val blockHeight = height.getOrElse(0)
    val heightBytes = Ints.toByteArray(blockHeight)
    val n = scheme.calcN(solverVersion, blockHeight)
    val started = System.nanoTime()
    val limitNanos = 30.seconds.toNanos
    val nonceLimit = 100000L
    var nonce = 0L
    while (nonce < nonceLimit && System.nanoTime() - started < limitNanos) {
      scheme.checkNonces(
        solverVersion, heightBytes, message, secret, BigInt(2), target, n,
        nonce, nonce + 1L, parameters
      ) match {
        case OrderingSolutionFound(solution) if !inputBlock => return solution
        case InputSolutionFound(solution) if inputBlock => return solution
        case _ => ()
      }
      nonce += 1L
    }
    val kind = if (inputBlock) "input block" else "ordering block"
    throw new IllegalStateException(
      s"No $kind solution found after $nonce nonces (limits: $nonceLimit nonces, 30 seconds)"
    )
  }
}
