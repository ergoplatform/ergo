package org.ergoplatform.http.api.requests

import org.ergoplatform.ErgoLikeContext

/**
  * Represent a request for execution of a script in a given context.
  *
  * @param script  ErgoScript source code of the contract to execute
  * @param env      environment map of named constants used to compile the script
  * @param ctx      script execution context
  */
case class ExecuteRequest(script: String,
                          env: Map[String,Any],
                          ctx: ErgoLikeContext)
