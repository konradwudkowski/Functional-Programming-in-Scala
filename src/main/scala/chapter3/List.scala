package chapter3

import scala.annotation.tailrec

trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Ex 3.2
  def tail[A](list: List[A]) = list match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // Ex 3.3
  def setHead[A](headReplacement: A, list: List[A]): List[A] = list match {
    case Cons(x, xs) => Cons(headReplacement, xs)
    case Nil => Nil
  }

  // Ex 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (n,l) match {
    case (ctr, _) if ctr <= 0 => l
    case (_, Cons(x, xs)) => drop(xs, n-1)
    case (_ , Nil) => Nil
  }

  // Ex 3.5
  @tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case Nil => Nil
    case _ => list
  }

  // Ex 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x,init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs,z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // Ex 3.7 product implemented using foldRight probably can't halt recursion and return 0.0
  // if it encounters a 0.0

  // Ex 3.8
  foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)) // this will evaluate to List(1,2,3)

  // Ex 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)( (_, acc) => acc + 1)

  // Ex 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    @tailrec
    def loop(l: List[A], prev: B): B = l match {
      case Nil => prev
      case Cons(x, xs) => loop( xs, f(prev,x) )
    }
    loop(as,z)
  }

  // Ex 3.11
  def sumUsingFL(xs: List[Int]): Int = foldLeft(xs,0)(_ + _)
  def productUsingFL(xs: List[Double]): Double = foldLeft(xs,1.0)(_ * _)
  def lengthUsingFL[A](xs: List[A]): Int = foldLeft(xs,0)( (acc,_) => acc + 1)

  // Ex 3.12
  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A])( (acc,current) => Cons(current,acc))

  // Ex 3.13
  def foldLeftUsingFR[A,B](xs: List[A], z: B)(f: (B,A) => B): B = foldRight(reverse(xs),z)( (a,b) => f(b,a) )
  def foldRightUsingFL[A,B](xs: List[A], z: B)(f: (A,B) => B): B = foldLeft(reverse(xs), z)( (b,a) => f(a,b) )

  // Ex 3.14
  def appendList[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys)( (current, acc) => Cons(current,acc))
  def appendElement[A](xs: List[A], x: A): List[A] = appendList(xs, List(x))

  // Ex 3.15
  def flatten[A](listOfLists: List[List[A]]): List[A] = foldLeft(listOfLists, Nil: List[A]) {
    (acc,currentList) => appendList(acc,currentList)
  }

  // Ex 3.16
  def add1(list: List[Int]): List[Int] =
    foldRight(list, Nil: List[Int])( (current,acc) => Cons(current + 1, acc))

  // Ex 3.17
  def doubleToString(list: List[Double]): List[String] =
    foldRight(list, Nil: List[String])( (current,acc) => Cons(current.toString, acc))

  // Ex 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])( (current,acc) => Cons( f(current), acc))

  // Ex 3.19
  def filter[A](as: List[A])(f: A => Boolean) =
    foldRight(as, Nil: List[A])( (current, acc) => if ( f(current) ) Cons(current, acc) else acc )

  // Ex 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B])( (acc, current) => appendList(acc,f(current)))

  // Ex 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean) = flatMap(as)(x => if(f(x)) List(x) else Nil)

  // Ex 3.22
  def addCorrespondingElements(firstList: List[Int], secondList: List[Int]): List[Int] =
    (firstList, secondList) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addCorrespondingElements(xs, ys))
    }

  // Ex 3.23
  def zipWith[A, B](firstList: List[A], secondList: List[A])(f: (A, A) => B): List[B] =
    (firstList, secondList) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  // Ex 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(x,xs) =>
      foldLeft(zipWith(sup,sub)(_ == _), true)( (acc, current) => current && acc) || hasSubsequence(xs,sub)
  }

}











