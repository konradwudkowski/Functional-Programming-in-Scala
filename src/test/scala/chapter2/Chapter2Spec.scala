package chapter2

import org.scalatest.{Matchers, FreeSpec}
import chapter2.Chapter2._

class Chapter2Spec extends FreeSpec with Matchers {
  "function `fib` should return correct fibonacci numbers" in {
    // 0 1 1 2 3 5 8 13 21 34 55 89
    fib(0) shouldBe 0
    fib(1) shouldBe 1
    fib(6) shouldBe 8
    fib(11) shouldBe 89
  }
}
