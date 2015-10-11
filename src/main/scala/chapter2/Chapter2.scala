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

}
