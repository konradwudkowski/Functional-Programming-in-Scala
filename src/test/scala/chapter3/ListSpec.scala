package chapter3

import org.scalatest.{Matchers, FreeSpec}

class ListSpec extends FreeSpec with Matchers {

  import List._

  // Ex 3.2
  "function `tail` should" - {
    "remove first element of a list" in {
      tail(List(1,2,3)) shouldBe List(2,3)
    }
    "return Nil if Nil was passed" in {
      tail(Nil) shouldBe Nil
    }
  }

  // Ex 3.3
  "function `setHead` should" - {
    "replace the first element of a list" in {
      setHead(5, List(1,2,3)) shouldBe List(5,2,3)
    }
    "return Nil if Nil was passed" in {
      setHead(5, Nil) shouldBe Nil
    }
  }

  // Ex 3.4
  "function `drop` should" - {
    "remove the first n elements of a list" in {
      drop(List(1,2,3), 1) shouldBe List(2,3)
      drop(List(1,2,3), 2) shouldBe List(3)
      drop(List(1,2,3), 3) shouldBe Nil
    }
    "return Nil if Nil was passed" in {
      drop(Nil, 1) shouldBe Nil
    }
  }

  // Ex 3.5
  "function `dropWhile` should" - {
    "remove elements from a list as long as the meet a predicate" in {
      dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x < 4) shouldBe List(4, 5, 6)
    }
    "return Nil if Nil was passed" in {
      dropWhile(Nil, identity) shouldBe Nil
    }
  }

  // Ex 3.6
  "function `init` should" - {
    "return all elements but the last one" in {
      init(List(1,2,3)) shouldBe List(1,2)
      List.init(List(1)) shouldBe Nil
    }
    "return Nil if Nil was passed" in {
      init(Nil) shouldBe Nil
    }
  }

  // Ex 3.9
  "function `length` should return the number of elements in a list" in {
    List.length(List(1,2,3)) shouldBe 3
    List.length(List(1)) shouldBe 1
    List.length(Nil) shouldBe 0
  }

  // Ex 3.10
  "function `foldRight` will throw StackOverflowException for a large list" in {
    intercept[StackOverflowError] {
      val largeList = List( (1 to 10000).toArray: _*) // current implementation of List won't work for big lists...
      foldRight( largeList, 0 )( (x,y) => 0 )
    }
  }
  "function `foldLeft` should work as foldRight on a reversed list" in {
    foldLeft(List(1,2,3),0)(_ + _) shouldBe foldRight(List(3,2,1),0)(_ + _)
    foldLeft(List(1,2,3),1.0)(_ * _) shouldBe foldRight(List(3,2,1),1.0)(_ * _)
  }

  // Ex 3.11
  "function `sumUsingFL` should use a foldLeft internally but generate the same number as `List.sum`" in {
    sumUsingFL(List(1,2,3)) shouldBe sum(List(1,2,3))
    sumUsingFL(Nil) shouldBe sum(Nil)
  }
  "function `productUsingFL` should use a foldLeft internally but generate the same number as `List.product`" in {
    productUsingFL(List(1,2,3)) shouldBe product(List(1,2,3))
    productUsingFL(Nil) shouldBe product(Nil)
  }
  "function `productUsingFL` should use a foldLeft internally but generate the same number as `List.length`" in {
    lengthUsingFL(List(1,2,3)) shouldBe List.length(List(1,2,3))
    lengthUsingFL(Nil) shouldBe List.length(Nil)
  }

  // Ex 3.12
  "function `reverse` should reverse a list" in {
    reverse(List(1,2,3)) shouldBe List(3,2,1)
    reverse(List(1)) shouldBe List(1)
    reverse(Nil) shouldBe Nil
  }

  // Ex 3.13
  "function `foldLeftUsingFR` should be implemented using foldRight but work as foldLeft" in {
    foldLeftUsingFR(List(1,2,3), 0)(_ + _) shouldBe foldLeft(List(1,2,3), 0)(_ + _)
    foldLeftUsingFR(List(1,2,3,4,5), 1.0)(_ * _) shouldBe foldLeft(List(1,2,3,4,5), 1.0)(_ * _)
  }
  "function `foldRightUsingFL` should be implemented using foldLeft but work as foldRight" in {
    foldRightUsingFL(List(1,2,3), 0)(_ + _) shouldBe foldRight(List(1,2,3), 0)(_ + _)
    foldRightUsingFL(List(1,2,3,4,5), 1.0)(_ * _) shouldBe foldRight(List(1,2,3,4,5), 1.0)(_ * _)
  }

  // Ex 3.14
  "function `append` should append elements to the end of a list" in {
    appendElement(List(1,2,3), 4) shouldBe List(1,2,3,4)
    appendElement(Nil, 1) shouldBe List(1)
  }

  // Ex 3.15
  "function `flatten` should concatenate a list of lists into a single list" in {
    flatten(List(List(1,2,3),List(4,5,6))) shouldBe List(1,2,3,4,5,6)
  }



}
