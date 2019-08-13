package org.ergoplatform.settings

final case class Args(userConfigPathOpt: Option[String],
                      networkIdOpt: Option[NetworkType])

object Args {
  def empty: Args = Args(None, None)
}
