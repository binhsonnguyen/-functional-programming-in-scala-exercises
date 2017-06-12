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
    * A more surprising example of data sharing is this function that adds all the elements of one list to
    * the end of another:
    */
  def append[A](a1: Listt[A], a2: Listt[A]): Listt[A] = a1 match {
    case Nill => a2
    case Cons(h, t) => Cons(h, append(t, a2))
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
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nill => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + Listt.sum(t)
  case _ => 101
}

