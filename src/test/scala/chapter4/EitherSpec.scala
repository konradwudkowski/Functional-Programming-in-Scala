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

}
