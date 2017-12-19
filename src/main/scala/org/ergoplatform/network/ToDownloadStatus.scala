package org.ergoplatform.network

import scorex.core.ModifierTypeId

case class ToDownloadStatus(tp: ModifierTypeId, firstViewed: Long, lastTry: Long)
