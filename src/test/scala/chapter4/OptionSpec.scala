package chapter4

import org.scalatest.{FreeSpec, Matchers}
import Option._
import scala.util.Try

class OptionSpec extends FreeSpec with Matchers {

  // Exercise 4.2
  "Function variance should return variance of a seq of doubles" in {
    variance(List()) shouldBe None
    variance(List(1,2,3,4)) shouldBe Some(1.25)
    variance(List(1,2,3,4,5,6,7)) shouldBe Some(4)
  }

  // Exercise 4.3
  "Function should combine two values inside Options using a binary function" in {
    map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
    map2(None: Option[Int], Some(1))(_ + _) shouldBe None
  }

  // Exercise 4.4
  "Function sequence should covert from a list of Options to an option of a list" in {
    sequence(List(Some(1), Some(2))) shouldBe Some(List(1,2))
    sequence(List(Some(1), None)) shouldBe None
  }

  // Exercise 4.5
  "Function traverse should traverse a list once, apply a function A => Option[B] " +
    "and short-circuit if f evaluates to None" in {
    val toInt: String => Option[Int] = s => try { Some(s.toInt)} catch { case _: Exception => None}
    traverse(List("1", "2", "3"))(toInt) shouldBe Some(List(1,2,3))
    traverse(List("1", "2", "foo"))(toInt) shouldBe None
  }

  "Sequence in terms of traverse should work exactly as regular sequence" in {
    sequence(List(Some(1), Some(2))) shouldBe sequenceViaTraverse(List(Some(1), Some(2)))
    sequence(List(Some(1), None)) shouldBe sequenceViaTraverse(List(Some(1), None))
  }

}
