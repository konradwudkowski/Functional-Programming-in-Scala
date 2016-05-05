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

  // Ex. 6.3
  // Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
  // already written.

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, nextRng) = rng.nextInt
    val (double, nextRng2) = RNG.double(nextRng)
    ((int,double), nextRng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int,double), rng2) = intDouble(rng)
    ((double,int), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rng1) = RNG.double(rng)
    val (double2, rng2) = RNG.double(rng1)
    val (double3, rng3) = RNG.double(rng2)
    ( (double1, double2, double3), rng3)
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
