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

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]