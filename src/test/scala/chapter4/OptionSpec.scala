package chapter4

import org.scalatest.{FreeSpec, Matchers}
import Option._

class OptionSpec extends FreeSpec with Matchers {

  // Exercise 4.2
  "Function variance should return variance of a seq of doubles" in {
    variance(List()) shouldBe None
    variance(List(1,2,3,4)) shouldBe Some(1.25)
    variance(List(1,2,3,4,5,6,7)) shouldBe Some(4)
  }

  // Exercise 4.4
  "Function sequence should covert from a list of Options to an option of a list" in {
    sequence(List(Some(1), Some(2))) shouldBe Some(List(1,2))
    sequence(List(Some(1), None)) shouldBe None
  }

}
