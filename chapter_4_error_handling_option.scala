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

/**
  * Người ta thường dễ đi đến kết luận là một khi đã bắt đầu sử dụng đến `Maybe` type, nó sẽ ảnh hưởng
  * đến toàn bộ code. Cứ thử tưởng tượng bất kì chỗ nào gọi một method mà trả về một `Maybe` sẽ đều
  * phải sửa để xử lý các `Some` và `None` là biết. Nhưng, điều đó không xảy ra. Là vì, chúng ta có thể
  * nâng những hàm bình thường lên thành những hàm hoạt động trên `Maybe`.
  *
  * Ví dụ, hàm `map` biến đổi `Maybe[A]` của chúng ta thành một `Maybe[B]` dựa trên một hàm `f: A => B`.
  * MỘT CÁCH NHÌN KHÁC về vấn đề này, là hàm `map` đã biến một hàm `f: A => B` thành hàm
  * `f': Maybe[A] => Maybe[B]`. Mã tường minh:
  */

def lift[A, B](f: A => B): Maybe[A] => Maybe[B] = _.map(f)


/**
  * Và thế là mọi hàm `f: A => B` đề có thể biến thành `f': Maybe[A] => Maybe[B]` chỉ với một thao tác
  * đơn giản:
  */
def mayABS: Maybe[Double] => Maybe[Double] = lift(math.abs)

/**
  * Giả sử chúng ta có một hàm định giá như sau.
  *
  * Top secret formula for computing an annual car
  * insurance premium from two key factors.
  */
def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double

/**
  * Vấn đề là hàm trên nhận thông tin mà được parse từ một webform, với ngữ cảnh đó thì các đầu vào
  * đều là `String`, và thế là cần phải được `parse` thành `Int` trước khi đưa vào tính, và việc parse
  * đó hoàn toàn có thể fail, và ta dính exception của hàm `toInt`.
  *
  * Có nghĩa là, việc tính ra kết quả không phải bao giờ cũng thực hiện được, cho nên hàm tính rate
  * là một hàm trả về một `Maybe`. Và việc Maybe đó None hay không phụ thuộc vào `toInt` có thành công
  * hay không - cũng là một `Maybe`. Ta cần chuyển toInt thành một hàm returnmaybealbe và áp dụng nó
  * vào hàm tính rate. Như sau:
  *
  */
def Try[A](f: => A): Maybe[A] = {
  try Some(f)
  catch {
    case _: Exception => None
  }
}
def insuranceRateQuote2(age: String, numberOfSpeedingTickets: String): Maybe[Double] = {
  val maybeAge = Try(age.toInt)
  val maybeTicket = Try(numberOfSpeedingTickets.toInt)
  insuranceRateQuote2(maybeAge, maybeTicket)
}
def insuranceRateQuote2(age: Maybe[Int], numberOfSpeedingTickets: Maybe[Int]): Maybe[Double]

/**
  * Như đã thấy, ta đã phải viết thêm một hàm `insuranceRateQuote` mới để xử lý trên 2 `Maybe`.
  * Điều đó không tốt. Cách tốt hơn là lift nó lên. Tương tự như lift 1, ta có thể dùng pattern
  * matching thay vì `map`, nhưng việc đó thật chán ngắt.
  *
  * EXERCISE 4.3
  *
  * Viết một hàm tổng quát `map2` để gom hai giá trị `Maybe` bằng một phép toán cho trước. Nếu một
  * trong hai `Maybe` là `None`, nó cũng trả về giá trị đó.
  */
def map2[A, B, C](ma: Maybe[A], mb: Maybe[B])(f: (A, B) => C): Maybe[C] =
  ma flatMap (a => mb map (b => f(a, b)))

/**
  * Giờ thì tính rate đã đơn giản hơn
  */
def insuranceRateQuote3(age: String, numberOfSpeedingTickets: String): Maybe[Double] = {
  val maybeAge = Try(age.toInt)
  val maybeTicket = Try(numberOfSpeedingTickets.toInt)
  map2(maybeAge, maybeTicket)(insuranceRateQuote)
}

/**
  * `map2` giúp ta không cần phải sửa bất kỳ hàm nào đang sử dụng 2 đầu vào để làm chúng tương
  * thích với `Maybe` nữa.
  *
  * Có khi nào ta nghĩ đến `map3`, `map4`, `mapx` ?
  *
  * EXERCISE 4.4
  *
  * Write a function sequence that combines a list of Options into one Option containing a list of
  * all the Some values in the original list. If the original list contains None even once, the result
  * of the function should be None; otherwise the result should be Some with a list of all the values.
  *
  * Viết một hàm `sequence` gộp một danh sách các `Maybe` thành một `Maybe` chứa một danh sách
  * của tất cả các giá trị của các `Some` trong danh sách cũ. Nếu danh sách cũ chứa thậm chí một `None`,
  * kết quả sẽ là `None`
  */
def sequence[A](lma: List[Maybe[A]]): Maybe[List[A]] = lma match {
  case Nil => Some(Nil)
  case h :: t => h flatMap (ha => sequence(t) map (ha :: _))
}

def sequenceViaMaping[A](lma: List[Maybe[A]]): Maybe[List[A]] =
  lma.foldRight(Some(Nil): Maybe[List[A]])(map2(_, _)(_ :: _))

/**
  * Đặt tình huống ta có thể sử dụng `sequence` để làm những việc như dưới đây
  */
def parseInts(ls: List[String]): Maybe[List[Int]] =
  sequence(ls map (s => Try(s.toInt)))

/**
  * Trông thì có vẻ ngon, nhưng thật ra ta vừa duyệt qua một list 2 lần. Những trường hợp như thế
  * rất nhiều, cho nên sẽ có hàm `traverse` như dưới đây
  *
  * EXERCISE 4.5
  */
def traverse[A, B](l: List[A])(f: A => Maybe[B]): Maybe[List[B]] =
  l.foldRight[Maybe[List[B]]](Some(Nil))((a, mlb) => map2(f(a), mlb)(_::_))

// and we even can implement sequence via traverse
def sequenceViaTraverse[A](lma: List[Maybe[A]]): Maybe[List[A]] =
  traverse(lma)(ma => ma)

/**
  * Bởi vì việc `lift` quá phổ biến, Scala đã được bổ sung một cú pháp gọi là comprehension với
  * mục đích mở rộng flatMap cũng như map. Sau đây là map2 được biểu diễn bởi comprehension
  *
  */
def map2Original[A, B, C](ma: Maybe[A], mb: Maybe[B])(f: (A, B) => C): Maybe[C] =
  ma flatMap(a => mb map (b => f(a, b)))

/**
  * mỗi binding sẽ được desurgar thành một lời gọi flatMap, riêng binding cuối cùng và biểu thức
  * sau yield sẽ được chuyển thành map. Bạn có thể thấy nếu map3, map4... thì sử dụng cú pháp
  * comprehension sẽ gọn gàng hơn nhiều.
  *
  * Với map, lift, sequence, traverce, comprehension, bạn đã có thể không cần sửa bất cứ hàm
  * nào để sử dụng chúng với Maybe nữa.
  */
def map2ViaComprehension[A, B, C](ma: Maybe[A], mb: Maybe[B])(f: (A, B) => C): Maybe[C] =
  for {
    a <- ma
    b <- mb
  } yield f(a, b)
