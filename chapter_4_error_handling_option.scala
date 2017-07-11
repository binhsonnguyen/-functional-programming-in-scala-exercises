/**
 * EXERCISE 4.1
 */
sealed trait Maybe[+A] {
  def map[B](f: A => B): Maybe[B] = this match {
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
    case None => Some(ob)
    case _ => this
  }

  def orElseViaViaMaybe[B >: A](ob: => B): Maybe[B] = map(a => Some(a)).getOrElse(None)

  def filter(f: A => Boolean): Maybe[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def filterViaMaybe(f: A => Boolean): Maybe[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Maybe[A]
case object None extends Maybe[Nothing]

/**
 * Mặc dù luôn có thể sử dụng phiên bản pattern matching bình thường, nhưng chúng ta được kỳ vọng nên
 * luôn sử dụng HOFs như ở các phiên bản hàm "Via...". Ta sẽ luyện tập việc này.
 *
 * EXERCISE 4.2
 *
 * Lợi dụng `flatMap`, triển khai hàm `variance` tính phương sai. Nếu trung bình của một sequence là m,
 * phương sai là trung bình của `math.pow(x - m, 2)` trên mỗi `x` của sequence.
 */
def mean(xs: Seq[Double]): Maybe[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Maybe[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
