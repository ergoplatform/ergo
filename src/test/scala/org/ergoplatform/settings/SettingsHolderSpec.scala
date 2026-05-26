package org.ergoplatform.settings

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.settings

import java.util.concurrent.atomic.AtomicReference

class SettingsHolderSpec extends ErgoCorePropertyTest {

  private def candidate(cap: Int): ErgoSettings =
    settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = cap))

  property("trySwap updates current and invokes onSwap on success") {
    val captured = new AtomicReference[Option[(ErgoSettings, ErgoSettings)]](None)
    val holder = new SettingsHolder(
      settings,
      _ => Right(()),
      (prev, curr) => captured.set(Some((prev, curr)))
    )

    val newSettings = candidate(54321)
    holder.trySwap(newSettings) shouldBe Right(newSettings)
    holder.current shouldBe newSettings
    captured.get() shouldBe Some((settings, newSettings))
  }

  property("trySwap leaves state unchanged and does not invoke onSwap on persister failure") {
    val captured = new AtomicReference[Option[(ErgoSettings, ErgoSettings)]](None)
    val err = PersistError.IoFailure("disk full")
    val holder = new SettingsHolder(
      settings,
      _ => Left(err),
      (prev, curr) => captured.set(Some((prev, curr)))
    )

    val result = holder.trySwap(candidate(99))
    result shouldBe Left(err)
    holder.current shouldBe settings
    captured.get() shouldBe None
  }

  property("concurrent trySwap calls serialize and end on one of the candidates") {
    val holder = new SettingsHolder(settings, _ => Right(()), (_, _) => ())
    val N = 50
    val swaps = (1 to N).map(i => candidate(1000 + i))
    val threads = swaps.map { s =>
      new Thread(() => { val _: Either[PersistError, ErgoSettings] = holder.trySwap(s) })
    }
    threads.foreach(_.start())
    threads.foreach(_.join())

    val finalCap = holder.current.nodeSettings.mempoolCapacity
    finalCap should (be >= 1001 and be <= (1000 + N))
  }

  property("readonly holder returns initial settings and rejects swaps with NoWritableConfig") {
    val holder = SettingsHolder.readonly(settings)
    holder.current shouldBe settings
    holder.trySwap(candidate(7)) shouldBe Left(PersistError.NoWritableConfig)
    holder.current shouldBe settings
  }
}
