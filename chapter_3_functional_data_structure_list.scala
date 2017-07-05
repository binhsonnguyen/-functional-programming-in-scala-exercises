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

  /**
    * EXERCISE 3.7
    *
    * Can `product2`, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a
    * 0.0? Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
    * This is a deeper question that we’ll return to in chapter 5.
    *
    * Hàm `product2` được implement bởi foldRight có thể nào tắt ngay lập tức và trả về 0.0 ngay khi gặp bất
    * kỳ gía trị 0.0 nào trong list không? Vì sao? Đặt vấn đề: làm thế nào để "đoản mạch" trong trường hợp ta
    * dùng foldRight trên một list quá lớn? Câu hỏi này sẽ được trở lại trong chương 5.
    *
    * ANSWER
    *
    * Chịu. Vì `f` sẽ duyệt hết list. Ta cần thêm syntax hay mechanic khác để có thể thực hiện được "đoản mạch"
    *
    */

  /**
    * EXERCISE 3.8
    *
    * See what happens when you pass Nil and Cons themselves to foldRight, like this:
    *
    * ```
    * foldRight(Listt(1,2,3), Nill:Listt[Int])(Cons(_,_))
    * ```
    *
    * What do you think this says about the relationship between foldRight and
    * the data constructors of List?
    *
    * ANSWER
    *
    * We get back the input list. Even more, if we replace `Nill` with another list, we was implement `concat` function
    */


  /**
    * EXERCISE 3.9
    *
    * Compute the length of a list using foldRight.
    */
  def length[A](l: Listt[A]): Int = foldRight(l, 0)((_, length) => length + 1)

  /**
    * EXERCISE 3.10
    *
    * Our implementation of foldRight is not tail-recursive and will result in a StackOver - flowError for large
    * lists (we say it’s not _stack-safe_). Convince yourself that this is the case, and then write another general
    * list-recursion function, `foldLeft`, that is tail-recursive, using the techniques we discussed in the previous
    * chapter. Here is its signature
    */
  @annotation.tailrec
  def foldLeft[A,B](l: Listt[A], z: B)(f: (B, A) => B): B = l match {
    case Nill => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * EXERCISE 3.11
    *
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */
  def sum3(l: Listt[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: Listt[Double]): Double = foldLeft(l, 0.0)(_ * _)
  def length2(l: Listt[Int]): Int = foldLeft(l, 0)((x, _) => x + 1)


  /**
    * EXERCISE 3.12
    *
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)). See if you
    * can write it using a fold.
    */
  def reverse[A](l: Listt[A]): Listt[A] = foldLeft(l, Nill: Listt[A])((as, x) => Cons(x, as))

  /**
    * EXERCISE 3.13
    *
    * Hard: Can you write foldLeft in terms of foldRight? How about the other way around? Implementing foldRight
    * via foldLeft is useful because it lets us implement foldRight tail-recursively, which means it works even
    * for large lists without overflow- ing the stack.
    */
  def foldRightViaFoldLeft[A, Z](l: Listt[A], z: Z)(f: (A, Z) => Z): Z = foldLeft(reverse(l), z)((z, a) => f(a, z))

  /**
    * EXERCISE 3.14
    *
    * Implement append in terms of either `foldLeft` or `foldRight`.
    */
  def appendViaFoldRight[A](l: Listt[A], r: Listt[A]): Listt[A] = {
    foldRightViaFoldLeft(l, r)(Cons(_, _))
  }

  /**
    * EXERCISE 3.15
    *
    * Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear
    * in the total length of all lists. Try to use functions we have already defined.
    */
  def concat[A](l: Listt[Listt[A]]): Listt[A] = {
    foldRightViaFoldLeft(l, Nill: Listt[A])(append)
  }

  /**
    * EXERCISE 3.16
    *
    * Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be
    * a pure function that returns a new List!)
    */
  def add1(l: Listt[Int]): Listt[Int] = foldRight(l, Nill: Listt[Int])((i, t) => Cons(i + 1, t))

  /**
    * EXERCISE 3.17
    *
    * Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString
    * to convert some d: Double to a String.
    */
  def mapToString(l: Listt[Double]): Listt[String] = foldRight(l, Nill: Listt[String])((d, t) => Cons(d.toString, t))

  /**
    * EXERCISE 3.18
    *
    * Write a function map that generalizes modifying each element in a list while maintain- ing the structure of
    * the list. Here is its signature:12
    */
  def map[A, B](l: Listt[A])(f: A => B): Listt[B] = foldRight(l, Nill: Listt[B])((a, t) => Cons(f(a), t))
  def map_2[A, B](l: Listt[A])(f: A => B): Listt[B] = foldRightViaFoldLeft(l, Nill: Listt[B])((a, t) => Cons(f(a), t))
  def map_3[A, B](l: Listt[A])(f: A=> B): Listt[B] = Nill

  /**
    * EXERCISE 3.20
    *
    * Write a function flatMap that works like map except that the function given will return a list instead of a
    * single result, and that list should be inserted into the final resulting list.
    *
    * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
    */
  def flatMap[A, B](l: Listt[A])(f: A => Listt[B]): Listt[B] = concat(map(l)(f))

  /**
    * EXERCISE 3.21
    *
    * Use `flatMap` to implement `filter`
    */
  def filter[A](l: Listt[A])(f: A => Boolean): Listt[A] = flatMap(l)(a => if (f(a)) Listt(a) else Nill)

  /**
    * other implementation of `filter`
    */
  def filter_2[A](l: Listt[A])(f: A => Boolean): Listt[A] =
    foldRight(l, Nill: Listt[A])((a, b) => if (f(a)) Cons(a, b) else b)
  def filter_3[A](l: Listt[A])(f: A => Boolean): Listt[A] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(l: Listt[A]): Unit = l match {
      case Nill => Unit
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    Listt(buf.toList: _*)
  }

  /**
    * EXERCISE 3.22
    *
    * Write a function that accepts two lists and constructs a new list by adding corresponding elements. For
    * example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  def addPair(l1: Listt[Int], l2: Listt[Int]): Listt[Int] = (l1, l2) match {
    case (Nill, _) => Nill
    case (_, Nill) => Nill
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPair(t1, t2))
  }

  /**
    * EXERCISE 3.23
    *
    * Generalize the function you just wrote so that it’s not specific to integers or addition. Name your
    * generalized function zipWith.
    */
  def zipWith[A, B](l1: Listt[A], l2: Listt[A])(f: (A, A) => B): Listt[B] = (l1, l2) match {
    case (Nill, _) => Nill
    case (_, Nill) => Nill
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /**
    * some function take from scala's List library
    */
  def take[A](l: Listt[A], n: Int): Listt[A] = {
    if (n <= 0) Nill
    else l match {
      case Nill => Nill
      case Cons(h, t) => Cons(h, take(t, n - 1))
    }
  }

  def takeViaListBuf[A](l: Listt[A], n: Int): Listt[A] = {
    if (n <= 0) Nill

    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(l: Listt[A], n: Int): Unit = l match {
      case Nill => Nill
      case Cons(h, t) => buf += h; go(t, n - 1)
    }
    go(l, n)

    Listt(buf.toList: _*)
  }

  def takeWhile[A](l: Listt[A])(f: A => Boolean): Listt[A] = l match {
    case Nill => Nill
    case Cons(h, t) => if (f(h)) Cons(h, takeWhile(t)(f)) else Nill
  }

  def takeWhileViaListBuf[A](l: Listt[A])(f: A => Boolean): Listt[A] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(l: Listt[A])(f: A => Boolean): Unit = l match {
      case Nill => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)(f)
    }
    go(l)(f)

    Listt(buf.toList: _*)
  }

  @annotation.tailrec
  def forall[A](l: Listt[A])(f: A => Boolean): Boolean = l match {
    case Nill => f(Nill)
    case Cons(h, t) => f(h) && forall(t)(f)
  }

  @annotation.tailrec
  def exists[A](l: Listt[A])(f: A => Boolean): Boolean = l match {
    case Nill => f(Nill)
    case Cons(h, t) => if (f(h)) true else exists(t)(f)
  }

  /**
    * EXERCISE 3.24
    *
    * Hard: As an example, implement `hasSubsequence` for checking whether a List contains another List as a
    * subsequence. For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences,
    * among others. You may have some difficulty finding a concise purely functional implementation that is
    * also efficient. That’s okay. Implement the function however comes most naturally. We’ll return to this
    * implementation in chapter 5 and hopefully improve on it. Note: Any two values x and y can be compared for
    * equality in Scala using the expression x == y.
    */
  def startWith[A](l: Listt[A], s: Listt[A]): Boolean = (l, s) match {
    case (_, Nill) => l == Nill
    case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 & startWith(t1, t2)
    case _ => false
  }

  def startWithViaBuf[A](l: Listt[A], s: Listt[A]): Boolean = {
    var buf = true
    @annotation.tailrec
    def go(l: Listt[A], s: Listt[A]): Unit = (l, s) match {
      case (_, Nill) => buf = l == Nill
      case (Cons(h1, t1), Cons(h2, t2)) => buf &= h1 == h2; go(t1, t2)
      case _ => buf = false
    }
    go(l, s)
    buf
  }

  @annotation.tailrec
  def hasSubsequence[A](l: Listt[A], s: Listt[A]): Boolean = s match {
    case Nill => l == Nill
    case _ if startWith(l, s) => true
    case Cons(h, t) => hasSubsequence(t, s)
  }

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

