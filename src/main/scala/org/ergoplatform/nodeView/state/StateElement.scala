package org.ergoplatform.nodeView.state

import scorex.core.serialization.{BytesSerializable, Serializer}

class StateElement extends BytesSerializable {
  override type M = StateElement

  override def serializer: Serializer[StateElement] = ???
}
