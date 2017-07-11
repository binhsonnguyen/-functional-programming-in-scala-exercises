/**
 * EXERCISE 4.1
 */
sealed trait MayBe[+A] {
  def map[B](f: A => B): B = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMapViaMaybe[B](f: A => Maybe[B]): Maybe[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => B): Maybe[B] = this match {
    case None => Maybe(ob)
    case _ => this
  }

  def orElseViaViaMaybe[B >: A](ob: => B): Maybe[B] = map(a => Some(a)).getOrElse(None)

  def filter(f: A => Boolean): Maybe[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

  def filterViaMaybe(f: A => Boolean): Maybe[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends MayBe[A]
case object None extends MayBe[Nothing]
