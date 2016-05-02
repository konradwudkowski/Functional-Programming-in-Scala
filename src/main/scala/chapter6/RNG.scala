package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def double(rng: RNG): (Double, RNG) = {
    val (randomInt, nextRng) = rng.nextInt
    ( math.abs(randomInt.toDouble) / Int.MaxValue, nextRng)
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
