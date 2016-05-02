package chapter6

import chapter6.RNG._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class RNGSpec extends FreeSpec with Matchers with PropertyChecks {

  // Ex. 6.1
  // Write a function to generate a Double between 0 and 1, not including 1. Note: You can
  // use Int.MaxValue to obtain the maximum positive integer value, and you can use
  // x.toDouble to convert an x: Int to a Double.

  "RNG::double" in {
    forAll { seed: Long =>
      val rng = new SimpleRNG(seed)
      val (result, _) = double(rng)
      val between0And1 = be >= 0.toDouble and be < 1.toDouble
      result should between0And1
    }
  }

}
