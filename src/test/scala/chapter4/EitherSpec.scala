package chapter4

import org.scalatest.{Matchers, FreeSpec}

class EitherSpec extends FreeSpec with Matchers {

  // Exercise 4.6

  "Either::map" in {
    Right("foo").map(_.toUpperCase) shouldBe Right("FOO")
    (Left("bar"): Either[String,String]).map(_.toUpperCase) shouldBe Left("bar")
  }
  "Either::flatMap" in {
    val f = (x: String) => try { Right(x.toInt) } catch { case ex: Exception => Left("error converting to int")}
    Right("foo").flatMap(f) shouldBe Left("error converting to int")
    Right("111").flatMap(f) shouldBe Right(111)
    Left("foo").flatMap(f)  shouldBe Left("foo")
  }
  "Either::orElse" in {
    Right("foo").orElse(Right("bar")) shouldBe Right("foo")
    Left("foo").orElse(Right("bar"))  shouldBe Right("bar")
    Right("foo").orElse(Left("bar"))  shouldBe Right("foo")
    Left("foo").orElse(Left("bar"))   shouldBe Left("bar")
  }
  "Either::map2" in {
    val f = (x1: String, x2: String) => x1 + x2
    Right("foo").map2(Right("bar"))(f) shouldBe Right("foobar")
    Right("foo").map2(Left("bar"))(f) shouldBe Left("bar")
    Left("foo").map2(Right("bar"))(f) shouldBe Left("foo")
    Left("foo").map2(Left("bar"))(f) shouldBe Left("foo")
  }

  // Exercise 4.7 implement sequence and traverse that return the first error encountered

  "Either::traverse" in {
    Either.traverse(List(Right(1), Right(2), Right(3)))(_.map(_ + 1)) shouldBe Right(List(2,3,4))
    Either.traverse(List(Right(1), Left(2)))(_.map(_ + 1)) shouldBe Left(2)

    Either.traverseUsingFoldRight(List(Right(1), Right(2), Right(3)))(_.map(_ + 1)) shouldBe Right(List(2,3,4))
    Either.traverseUsingFoldRight(List(Right(1), Left(2)))(_.map(_ + 1)) shouldBe Left(2)
  }

  "Either::sequence" in {
    Either.sequence(List(Right(1),Right(2))) shouldBe Right(List(1,2))
    Either.sequence(List(Right(1),Left(2))) shouldBe Left(2)
  }

}
