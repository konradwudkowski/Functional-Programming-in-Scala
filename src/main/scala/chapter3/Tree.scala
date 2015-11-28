package chapter3

sealed trait Tree [+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Ex 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  // Ex 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l).max(maximum(r))
  }

  // Ex 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + depth(l).max(depth(r))
  }

  // Ex 3.28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  // Ex 3.29
  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l,r) => g( fold(l)(f)(g), fold(r)(f)(g) )
  }

  def sizeUsingFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)( (sizeL,sizeR) => 1 + sizeL + sizeR)

  def maximumUsingFold(tree: Tree[Int]): Int =
    fold(tree)(identity)( (maxL,maxR) => maxL.max(maxR) )

  def depthUsingFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)( (depthL, depthR) => 1 + depthL.max(depthR) )

  def mapUsingFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B]) { (l,r) => Branch(l,r) }

}


