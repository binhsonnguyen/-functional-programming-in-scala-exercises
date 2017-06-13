sealed trait Listt[+A]

case object Nill extends Listt[Nothing]

case class Cons[+A](head: A, tail: Listt[A]) extends Listt[A]


/**
  * Object này, mặc dù có thể đặt bằng tên khác cho tường minh, nhưng đã được cố ý đặt trùng tên với trait `Listt`.
  * Scala cung cấp luật chơi này với cả `trait` lẫn `class`. Object được đặt tên như thế gọi là `companion object`.
  * Nó sẽ cung cấp các method trông giống như là static method của trait/class.
  *
  * PS: lúc tôi soạn comment trên thì tôi để ý rằng scala không có từ khoá static.
  *
  * Câu hỏi là: "tại sao scala không cung cấp game mechanic `static` ?"
  *
  * Câu trả lời có vẻ nằm ở chỗ: tất cả các ngôn ngữ hướng hàm đều có xu hưởng không dùng tới global state.
  * Vì nó tạo điều kiện để việc vi phạm side effect.
  *
  * Tuy nhiên, sau khi lý giải như thế thì tôi không thể thoát ra khỏi suy nghĩ: companion object
  * thực sự là một global state. Qua tìm hiều thì điều này thậm chí đã được xác nhận bởi `Gilad Bracha ` -
  * cha đẻ của Java Generics.
  * (https://gbracha.blogspot.com/2008/02/cutting-out-static.html)
  *
  * Vậy thì câu hỏi tiếp theo là: tại sao một ngôn ngữ muốn hướng hàm như scala lại cung cấp một thứ `static like` ?.
  *
  * Câu trả lời có lẽ nằm ở chỗ "Java có mechanic static". Và trong quá trình interoperability với Java thì
  * companion object buộc phải sinh ra. Có lẽ là sẽ còn nhiều thứ khác "buộc phải" sinh ra như vậy nữa chứ không chỉ
  * mình cái này - quá khó cho đội scala.
  *
  * Tổng kết: tôi đang học pure functional programing by Scala, nhưng tính pure ở đây rất dễ bị vi phạm, hãy cẩn thận.
  *
  */
object Listt {

  /**
    * ở đây đã sử dụng symbol `_`, có vẻ như nó được sử dụng để thay thế cho `anything`, hay `anything you give me`.
    * Magic của symbol này có vẻ đến từ mechanic pattern matching, hay sâu xa hơn là implicit của Scala.
    *
    * `A*` là dấu hiệu của một `variadic function` - chỉ ra rằng hàm này nhận 0 hoặc nhiều tham số.
    * Việc `as.isEmpty` mà không bị lỗi Runtime chỉ ra rằng chuỗi các tham số sẽ được "treat" vào một object khác
    * trước khi chạy vào thân hàm. Và thực tế thì chúng thật sự được gom vào một `Seq` object. Trong trường hợp này
    * là `Seq[A]`
    */
  def apply[A](as: A*): Listt[A] = {
    if (as.isEmpty) Nill
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: Listt[Int]): Int = ints match {
    case Nill => 0
    case Cons(h: Int, t: Listt[Int]) => h + sum(t) // first time we met partern matching
  }

  def product(ds: Listt[Double]): Double = ds match {
    case Nill => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  def fill[A](n: Int, a: A): Listt[A] = {
    if (n <= 0) Nill
    else Cons(a, fill(n - 1, a))
  }

  /**
    * Có một dòng ở đây cần được noted lại:
    * <quote>We say that functional data structures are persistent, meaning that existing references are never changed
    * by operations on  the data structure.</quote>
    */

  /**
    * EXERCISE 3.2
    * Implement the function tail for removing the first element of a List. Note that the function takes constant
    * time. What are different choices you could make in your implementation if the List is Nil? We’ll return
    * to this question in the next chapter.
    */
  def tail[A](l: Listt[A]): Listt[A] = l match {
    case Nill => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  /**
    * EXERCISE 3.3
    * Using the same idea, implement the function setHead for replacing the first element of a List with a
    * different value.
    */
  def setHead[A](l: Listt[A], h: A): Listt[A] = l match {
    case Nill => sys.error("set head on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * EXERCISE 3.4
    * Generalize tail to the function drop, which removes the first `n` elements from a list. Note that this
    * function takes time proportional only to the number of elements being dropped—we don’t need to make
    * a copy of the entire List.
    */
  def drop[A](l: Listt[A], n: Int): Listt[A] = {
    if (n <= 0) l
    else l match {
      case Nill => Nill
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  /**
    * EXERCISE 3.5
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    *
    * Note:
    * This is first time we used pattern matching's guard (`case _ if _`), it better than place `if` statement
    * after the `=>` because the pattern wont match if the guard not `true`
    * As long as first time using case others (`case _`)
    */
  def dropWhile[A](l: Listt[A], f: A => Boolean): Listt[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  /**
    * Cải tiến khả năng tự luận kiểu cho hàm cấp cao.
    *
    * Hàm `dropWhile` trên kia thường được truyền vào một hàm vô danh làm tham số thứ hai. Ví dụ
    * ```
    * val xs: Listt[Int] = Listt(1, 2, 3, 4)
    * val ex = dropWhilte(xs, (x: Int) => x < 4)
    * ```
    * Thật bất hạnh khi chúng ta phải định kiểu cho tham số `x` trong khi nó đáng lẽ có thể suy ra được
    * khi mà đầu vào của chúng ta là list số nguyên. Chúng ta có thể lợi dụng khả năng ngầm định kiểu của Scala
    * nếu tách các tham số của hàm `dropWhile` thành 2 nhóm như triển khai bên dưới đây. Phép này được gọi
    * bằng khái niệm "curry hoá".
    *
    * Chỉ khi group như vậy, thông tin về kiểu mới được truyền từ group đầu tiên lần lượt tới các group tiếp
    * theo, tạo điều kiện để tự luận kiểu, nhờ đó  `dropWhile2`, không cần chỉ định kiểu cho `x` nữa:
    * `val ex = dropWhile2(xs)(x => x < 4)`
    *
    * Thật ra thì khả năng tự luận kiểu của Scala compiler có hạn chế nên mới phải curry hoá. Chứ các ngôn ngữ
    * như Haskell hay OCaml có khả năng tự luận hoàn chỉnh hơn nhiều, nên rất ít khi phải định kiểu.
    */
  def dropWhile2[A](as: Listt[A])(f: A => Boolean): Listt[A] = as match {
    case Cons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => as
  }


  /**
    * A more surprising example of data sharing is this function that adds all the elements of one list to
    * the end of another:
    */
  def append[A](a1: Listt[A], a2: Listt[A]): Listt[A] = a1 match {
    case Nill => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
    * EXERCISE 3.6
    * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all
    * but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can’t this
    * function be implemented in constant time like tail?
    */
  def init[A](l: Listt[A]): Listt[A] = l match {
    case Nill => sys.error("init of an empty list")
    case Cons(_, Nill) => Nill
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
    * init2 using a list buffer
    */
  def init2[A](l: Listt[A]): Listt[A] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(c: Listt[A]): Listt[A] = c match {
      case Nill => sys.error("init of an empty list")
      case Cons(_, Nill) => Listt(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  /**
    * Đặt vấn đề ở hàm `sum` cũng như `product`, trong ngữ cảnh của `Generic` - có nghĩa là không quan tâm
    * tới sự khác nhau của kiểu trả về, thì chúng chỉ khác nhau ở "giá trị trả về khi đưa vào list rỗng" và
    * "phép toán hợp nhất để đưa đến kết quả". Mỗi khi ta gặp mã lặp theo kiểu như thế này, ta có thể khái
    * quát hoá chúng bằng cách đưa những biểu thức phụ (giá trị trả về khi list rỗng + phép toán hợp, trong
    * trường hợp vừa rồi) thành tham số của hàm.
    *
    * Giờ ta sẽ phân tích.
    *
    * Với trường hợp "giá trị trả về khi list rỗng", nó rất đơn giản là sẽ thành một tham số thứ hai bên cạnh
    * tham số list đầu vào.
    *
    * Tuy nhiên với "phép toán hợp" thì khác. Nó phụ thuộc vào những biến nội hạt khác của hàm. Do đó không
    * có cách nào khác nó phải thành một hàm nhận những biến nội hạt kia vào như những đối số.
    *
    * Kết quả ta có quá trình khai triển interface hàm như sau:
    * ```
    * def foldRight[A](as: Listt[A])
    * def foldRight[A, B](l: Listt[A]): B
    * def foldRight[A, B](l: Listt[A], z: B): B
    * def foldRight[A, B](l: Listt[A], z: B, f: (A, B) => B): B
    * def foldRight[A, B](l: Listt[A], z: B)(f: (A, B) => B): B
    * ```
    */
  def foldRight[A, B](l: Listt[A], z: B)(f: (A, B) => B): B = l match {
    case Nill => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(l: Listt[Int]): Int = foldRight(l, 0)(_ + _)
  def product2(l: Listt[Double]): Double = foldRight(l, 1.0)(_ * _)
}

/**
  * EXERCISE 3.1
  * What will be the result of the following match expression?
  *
  * ANSWER
  * 3
  */
val x = Listt(1, 2, 3, 4, 5) match {
  case Cons(h, Cons(2, Cons(4, _))) => h
  case Nill => 42
  case Cons(h, Cons(y, Cons(3, Cons(4, _)))) => h + y
  case Cons(h, t) => h + Listt.sum(t)
  case _ => 101
}

