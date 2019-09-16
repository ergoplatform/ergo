package org.ergoplatform.settings

final case class PoPowSettings(
  prove: Boolean,
  params: PoPowParams
)