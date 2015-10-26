
package chapter2

import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int =  {
      @tailrec
      def go(first: Int, second: Int, ctr: Int): Int = {
        if (ctr == n) second else go(second, first + second, ctr + 1)
      }
      if (n != 0 ) go(0,1,1) else 0
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(ctr: Int): Boolean = {
      if (ctr == as.length) {
        true
      } else {
        if (ordered(as(ctr-1),as(ctr))) go(ctr + 1) else false
      }
    }
    if (as.length == 0 || as.length == 1) true else go(1)
  }

  // alternative version without manual recursion
  def isSorted2[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = as.sliding(2).forall {
    case Array(first,second) => ordered(first,second)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))


}
