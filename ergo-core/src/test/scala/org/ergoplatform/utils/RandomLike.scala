package org.ergoplatform.utils

import scala.util.Random

trait RandomLike {
  def nextInt(i: Int): Int
  def nextBoolean(): Boolean
  def nextLong(): Long
  def nextDouble(): Double
}

class RandomWrapper(seed: Option[Int] = None) extends RandomLike {
  private[this] val rnd = seed.fold(new Random)(s => new Random(s))
  override def nextInt(i: Int): Int = rnd.nextInt(i)
  override def nextBoolean(): Boolean = rnd.nextBoolean()
  override def nextLong(): Long = rnd.nextLong()
  override def nextDouble(): Double = rnd.nextDouble()
}
