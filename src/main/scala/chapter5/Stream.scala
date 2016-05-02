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

  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold( this ) {
    case Empty => None
    case Cons(h,t) => Some( f(h()), t() )
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold( (this,n) ) {
    case (Cons(h,t), x) if x > 0 => Some( h(), (t(), x-1) )
    case _ => None
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h,t) if f(h()) => Some( h(), t() )
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold( (this,s2) ) {
    case ( Cons(h,t), Cons(s2head, s2tail) ) => Some( (Some(h()), Some(s2head())) -> (t(), s2tail()) )
    case ( Cons(h,t), Empty ) => Some( (Some(h()), None) -> (t(), Empty) )
    case ( Empty, Cons(s2head, s2tail) ) => Some( (None, Some(s2head())) -> (Empty, s2tail()) )
    case _ => None
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = (this zipAll s).forAll {
    case (Some(self), Some(other)) => self == other
    case (None, _) => false
    case _ => true
  }

  def tails(): Stream[Stream[A]] = unfold(this) {
    case s @ Cons(h,t) => Some( (s, t()) )
    case _ => None
  } append Stream.empty

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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def next(a: Int, b: Int): Stream[Int] = Stream.cons(a, next(b, a + b))
    next(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((next,state)) => Stream.cons(next, unfold(state)(f))
    case None => Stream.empty
  }

  def fibsViaUnfold: Stream[Int] = unfold( (0,1) ){
    case (x,y) => Some(x, (y, x + y))
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n){ n => Some(n, n + 1) }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a){ a => Some(a,a) }

  def onesViaUnfold: Stream[Int] = unfold(1){ n => Some(1,1)}

  def zipWith[A, B](first: Stream[A], second: Stream[A])(f: (A, A) => B): Stream[B] = unfold( first -> second) {
    case ( Cons(x,xs), Cons(y,ys) ) => Some( f(x(),y()), xs() -> ys() )
    case _ => None
  }

}
