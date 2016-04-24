package chapter5

import chapter5.Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(h, t) => h() :: t().toList
  }

  def take2(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if t() == empty => p(h())
    case Cons(h,t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAllUsingFoldRight(p: A => Boolean): Boolean = this.foldRight(true) {
    case (current, acc) => p(current) && acc
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(empty[A]) {
      (current,acc) => if (p(current)) cons(current,acc) else acc
    }

  def headOptionUsingFoldRight: Option[A] = this.foldRight(Option.empty[A]) {
    case (current, _) => Some(current)
  }

  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B]) {
    case (current,acc) => cons(f(current), acc)
  }

  def filter(p: A => Boolean): Stream[A] = this.foldRight(empty[A]) {
    case (current, acc) => if (p(current)) cons(current,acc) else acc
  }

  def append[B >: A](a: B): Stream[B] = this.foldRight(cons(a, empty[B])) {
    case (current, acc) => cons(current,acc)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight(s){
    case (current, acc) => cons(current, acc)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B]) {
    case (current, acc) => f(current).append(acc)
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
