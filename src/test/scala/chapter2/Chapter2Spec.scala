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

  "function `isSorted` should" - {
    "check whether array is sorted according to a given comparison function" in {
      def orderedInt(x1: Int, x2: Int): Boolean = x1 <= x2
      isSorted(Array(1,2,3,4,5,6), orderedInt) shouldBe true
      isSorted(Array(1,3,3), orderedInt) shouldBe true
      isSorted(Array(1,4,3), orderedInt) shouldBe false

      def stringLengthOrdered(s1: String, s2: String) = s1.length <= s2.length
      isSorted(Array("konrad","konrad1", "konrad123"), stringLengthOrdered) shouldBe true
      isSorted(Array("konrad1","konrad1", "konrad123"), stringLengthOrdered) shouldBe true
      isSorted(Array("konrad12","konrad1", "konrad123"), stringLengthOrdered) shouldBe false
    }
    "work for empty arrays" in {
      def orderedInt(x1: Int, x2: Int): Boolean = true
      isSorted[Int](Array(),orderedInt) shouldBe true
    }
  }

  "function `isSorted2` should" - {
    "check whether array is sorted according to a given comparison function" in {
      def orderedInt(x1: Int, x2: Int): Boolean = x1 <= x2
      isSorted2(Array(1,2,3,4,5,6), orderedInt) shouldBe true
      isSorted2(Array(1,3,3), orderedInt) shouldBe true
      isSorted2(Array(1,4,3), orderedInt) shouldBe false

      def stringLengthOrdered(s1: String, s2: String) = s1.length <= s2.length
      isSorted2(Array("konrad","konrad1", "konrad123"), stringLengthOrdered) shouldBe true
      isSorted2(Array("konrad1","konrad1", "konrad123"), stringLengthOrdered) shouldBe true
      isSorted2(Array("konrad12","konrad1", "konrad123"), stringLengthOrdered) shouldBe false
    }
    "work for empty arrays" in {
      def orderedInt(x1: Int, x2: Int): Boolean = true
      isSorted2[Int](Array(),orderedInt) shouldBe true
    }
  }
}
