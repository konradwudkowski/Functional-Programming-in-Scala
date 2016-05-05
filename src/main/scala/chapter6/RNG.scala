package chapter6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def double(rng: RNG): (Double, RNG) = {
    val (randomInt, nextRng) = rng.nextInt
    ( math.abs(randomInt.toDouble) / Int.MaxValue, nextRng)
  }
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInt, nextRng) = rng.nextInt
    if (randomInt != Int.MinValue) {
      math.abs(randomInt) -> nextRng
    } else {
      nonNegativeInt(nextRng)
    }
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
      (n, nextRNG)
  }
  scala.util.Random
}
