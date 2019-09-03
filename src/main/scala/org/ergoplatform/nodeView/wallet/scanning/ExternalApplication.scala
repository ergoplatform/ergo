package org.ergoplatform.nodeView.wallet.scanning

import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}


case class ExternalApplication(appId: Long, appName: String, trackingRule: ScanningPredicate)

case class ExternalAppRequest (appName: String, trackingRule: ScanningPredicate)

object ExternalApplicationSerializer extends ScorexSerializer[ExternalApplication] {
  override def serialize(obj: ExternalApplication, w: Writer): Unit = {
    w.putLong(obj.appId)
    w.putShortString(obj.appName)
    ScanningPredicateSerializer.serialize(obj.trackingRule, w)
  }

  override def parse(r: Reader): ExternalApplication = {
    val appId = r.getLong()
    val appName = r.getShortString()
    val sp = ScanningPredicateSerializer.parse(r)
    ExternalApplication(appId, appName, sp)
  }
}