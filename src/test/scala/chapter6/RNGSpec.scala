package chapter6

import chapter6.RNG._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class RNGSpec extends FreeSpec with Matchers with PropertyChecks {

  // Ex. 6.1
  // Write a function that uses RNG.nextInt to generate a random integer between 0 and
  // Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
  // Int.MinValue , which doesn’t have a non-negative counterpart.

  "RNG::nonNegativeInt" in {
    forAll { seed: Long =>
      val rng = new SimpleRNG(seed)
      val (result, _) = nonNegativeInt(rng)
      result should be >= 0
    }
  }

  // Ex. 6.2
  // Write a function to generate a Double between 0 and 1, not including 1. Note: You can
  // use Int.MaxValue to obtain the maximum positive integer value, and you can use
  // x.toDouble to convert an x: Int to a Double.

  "RNG::double" in {
    forAll { seed: Long =>
      val rng = new SimpleRNG(seed)
      val (result, _) = double(rng)
      val beBetween0And1 = be >= 0.toDouble and be < 1.toDouble
      result should beBetween0And1
    }
  }

  // Ex. 6.4
  // Write a function to generate a list of random integers.
  "RNG::ints" in {
    val ints = Gen.choose(-50, 1000)
    val longs = Arbitrary.arbitrary[Long]
    var counter = 0
    forAll(ints, longs) { (count: Int, seed: Long) =>
      counter += 1
      val size = RNG.ints(count)(new SimpleRNG(seed))._1.size
      if (count <= 0) {
        size shouldBe 0
      } else {
        size shouldBe count
      }
    }
  }






}
