package chapter5

import org.scalatest.{Matchers, FreeSpec}

class StreamSpec extends FreeSpec with Matchers {

  // Ex. 5.1

  "Stream::toList should force evaluation of all elements of a stream and return a List" in {
    Stream(1,2,3,4,5).toList shouldBe List(1,2,3,4,5)
    Empty.toList shouldBe List()
  }

  // Ex. 5.2
  // Write the function take(n) for returning the first n elements of a Stream, and
  // drop(n) for skipping the first n elements of a Stream .

  "Stream::take" in {
    Stream(1,2,3,4,5).take(3).toList shouldBe Stream(1,2,3).toList
    Stream(1,2,3,4,5).take(1).toList shouldBe Stream(1).toList
    Stream(1,2,3,4,5).take(100).toList shouldBe Stream(1,2,3,4,5).toList
    Stream(1,2,3,4,5).take(0).toList shouldBe List()
    Stream(1,2,3,4,5).take(-5).toList shouldBe List()
    Stream.cons(0, ???).take(1).toList shouldBe Stream(0).toList
  }

  "Stream::drop" in {
    Stream(1,2,3,4,5).drop(3).toList shouldBe Stream(4,5).toList
    Stream(1,2,3,4,5).drop(100).toList shouldBe List()
    Stream(1,2,3,4,5).drop(0).toList shouldBe Stream(1,2,3,4,5).toList
    Stream(1,2,3,4,5).drop(-5).toList shouldBe Stream(1,2,3,4,5).toList
  }

  // Ex. 5.3
  // Write the function takeWhile for returning all starting elements of a Stream that
  // match the given predicate.

  "Stream::takeWhile" in {
    Stream(1,2,3,4,5).takeWhile(_ < 4).toList shouldBe List(1,2,3)
    Stream(1,2,3,4,5).takeWhile(_ > 10).toList shouldBe List()
    Stream.empty[Int].takeWhile(_ > 1).toList shouldBe List()
  }

  // Ex. 5.4
  // Implement forAll , which checks that all elements in the Stream match a given
  // predicate. Your implementation should terminate the traversal as soon as it
  // encounters a nonmatching value.

  "Stream::forAll" in {
    Stream(1,1,1,1).forAll(_ == 1) shouldBe true
    Stream(1,1,1,0).forAll(_ == 1) shouldBe false
    Stream.empty[Int].forAll(_ == 1) shouldBe true

    Stream(1,1,1,1).forAllUsingFoldRight(_ == 1) shouldBe true
    Stream(1,1,1,0).forAllUsingFoldRight(_ == 1) shouldBe false
    Stream.empty[Int].forAllUsingFoldRight(_ == 1) shouldBe true
  }

  // Ex. 5.5
  // Use foldRight to implement takeWhile.

  "Stream::takeWhileUsingFoldRight" in {
    Stream(1,2,3,4,5).takeWhileUsingFoldRight(_ < 4).toList shouldBe List(1,2,3)
    Stream(1,2,3,4,5).takeWhileUsingFoldRight(_ > 10).toList shouldBe List()
    Stream.empty[Int].takeWhileUsingFoldRight(_ > 1).toList shouldBe List()
  }

  // Ex. 5.6
  // Hard: Implement headOption using foldRight

  "Stream::headOptionUsingFoldRight" in {
    Stream(1).headOptionUsingFoldRight shouldBe Some(1)
    Stream.empty[Int].headOptionUsingFoldRight shouldBe None
  }

  // Ex. 5.7
  // Implement map, filter, append, and flatMap using foldRight. The append method
  // should be non-strict in its argument.

  "Stream::map" in {
    Stream(1,2,3).map(_ + 10).toList shouldBe List(11,12,13)
    Stream.empty[Int].map(_ + 10).toList shouldBe List()
  }

  "Stream::filter" in {
    Stream(1,2,3,4,5).filter(_ % 2 == 0).toList shouldBe List(2,4)
    Stream.empty[Int].filter(_ % 2 == 0).toList shouldBe List()
  }

  "Stream::append(b)" in {
    Stream(1,2,3,4).append(5).toList shouldBe List(1,2,3,4,5)
    Stream.empty[Int].append(0).toList shouldBe List(0)
  }

  "Stream::append(Stream(b))" in {
    Stream(1,2,3,4).append(Stream(5,6)).toList shouldBe List(1,2,3,4,5,6)
    Stream.empty[Int].append(Stream(0)).toList shouldBe List(0)
  }

  "Stream::flatMap" in {
    Stream(1,2,3).flatMap(n => Stream(n, n * n)).toList shouldBe List(1, 1, 2, 4, 3, 9)
    Stream.empty[Int].flatMap(n => Stream(n, n * n)).toList shouldBe List()
  }

  // Ex 5.8
  // Generalize ones slightly to the function constant, which returns an infinite Stream of
  // a given value.

  "Stream::constant" in {
    Stream.constant(5).take(5).toList shouldBe List(5,5,5,5,5)
    Stream.constant("konrad").take(2).toList shouldBe List("konrad","konrad")
  }

  // Ex 5.9
  // Write a function that generates an infinite stream of integers, starting from n, then n
  // + 1, n + 2, and so on. 7

  "Stream::from" in {
    Stream.from(1).take(3).toList shouldBe List(1,2,3)
    Stream.from(-5).take(6).toList shouldBe List(-5, -4, -3, -2, -1, 0)
  }

  // Ex 5.10
  // Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
  // 2, 3, 5, 8, and so on.

  "Stream::fibs" in {
    Stream.fibs.take(11).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  }

  // Ex 5.11
  // Write a more general stream-building function called unfold. It takes an initial state,
  // and a function for producing both the next state and the next value in the generated
  // stream.

  "Stream::unfold" in {
    Stream.unfold(5)( n => Some( (n*2,n*2) )).take(3).toList shouldBe List(10,20,40)
  }

  // Ex. 5.12 Write fibs, from, constant, and ones in terms of unfold.

  "Stream::fibsViaUnfold" in {
    Stream.fibsViaUnfold.take(11).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  }

  "Stream::fromViaUnfold" in {
    Stream.fromViaUnfold(1).take(3).toList shouldBe List(1,2,3)
    Stream.fromViaUnfold(-5).take(6).toList shouldBe List(-5, -4, -3, -2, -1, 0)
  }

  "Stream::constantViaUnfold" in {
    Stream.constantViaUnfold(5).take(5).toList shouldBe List(5,5,5,5,5)
    Stream.constantViaUnfold("konrad").take(2).toList shouldBe List("konrad","konrad")
  }

  "Stream::onesViaUnfold" in {
    Stream.onesViaUnfold.take(5).toList shouldBe List(1,1,1,1,1)
  }

  // Ex. 5.13
  // Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and
  // zipAll. The zipAll function should continue the traversal as long as either stream
  // has more elements—it uses Option to indicate whether each stream has been
  // exhausted.

  "Stream::mapViaUnfold" in {
    Stream(1,2,3,4,5).mapViaUnfold(_ * 10).toList shouldBe List(10,20,30,40,50)
  }

  "Stream::takeViaUnfold" in {
    Stream(1,2,3,4,5).takeViaUnfold(3).toList shouldBe List(1,2,3)
  }

  "Stream::takeWhileViaUnfold" in {
    Stream(1,2,3,4,5).takeWhileViaUnfold(_ < 4).toList shouldBe List(1,2,3)
  }

  "Stream::zipWith" in {
    Stream.zipWith(Stream(1,2,3,4,5), Stream(10,20,30))( (a,b) => a + b ).toList shouldBe List(11,22,33)
  }

  "Stream::zipAll" in {
    Stream(1,2,3).zipAll( Stream(10) ).toList shouldBe List( Some(1) -> Some(10), Some(2) -> None, Some(3) -> None )
    Stream(1).zipAll( Stream(10,20,30) ).toList shouldBe List( Some(1) -> Some(10), None -> Some(20), None -> Some(30) )
  }

  // Ex. 5.14
  // Hard: Implement startsWith using functions you’ve written. It should check if one
  // Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
  // would be true.

  "Stream::startsWith" in {
    Stream(1,2,3) startsWith Stream(1,2) shouldBe true
    Stream(1,2,3) startsWith Stream(2) shouldBe false
    Stream(1,2,3) startsWith Stream(1,2,3,4) shouldBe false
  }

  // Ex 5.15
  // Implement tails using unfold. For a given Stream, tails returns the Stream of suf-
  // fixes of the input sequence, starting with the original Stream. For example, given
  // Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3),
  // Stream()) .

  "Stream::tails" in {
    pending // how to assert this? overridden toString?
    Stream(1,2,3).tails() shouldBe Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
  }

}
