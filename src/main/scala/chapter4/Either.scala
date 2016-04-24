package chapter4

trait Either[+E, +A] extends Product with Serializable {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case left @ Left(_) => left
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case left @ Left(_) => left
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case right @ Right(_) => this
    case left @ Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this,b) match {
    case ( Right(x), Right(y) ) => Right( f(x,y) )
    case ( Right(x), left @ Left(_) ) => left
    case ( left @ Left(x), _ ) => left
  }

}

object Either {
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case head :: tail => f(head).map2(traverse(tail)(f))(_ :: _)
  }

  def traverseUsingFoldRight[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List.empty[B]): Either[E, List[B]]) {
      (current, acc) => f(current).map2(acc)(_ :: _)
    }
  
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]