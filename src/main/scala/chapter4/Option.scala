package chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some.apply) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => Some(get)
    case _ => None
  }


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Functions {
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(ys: Seq[Double]) = if(ys.isEmpty) None else Some( ys.sum / ys.length )
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x-m,2)))
    }
  }
}

