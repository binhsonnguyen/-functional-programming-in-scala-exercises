/**
  * Algebraic Data Type
  * -------------------
  *
  * List is just one example of what’s called an algebraic data type (ADT). (Somewhat con- fusingly, ADT is
  * sometimes used elsewhere to stand for abstract data type.) An ADT is just a data type defined by one or
  * more data constructors, each of which may contain zero or more arguments. We say that the data type is
  * the sum or union of its data construc- tors, and each data constructor is the product of its arguments,
  * hence the name alge- braic data type.
  *
  * `List` là một ví dụ của thứ gọi là *kiểu dữ liệu đại số (ADT)*. (Hơi rắc rối một chút, nhưng ADT đôi
  * khi được dùng để thay thế cho kiểu dữ liệu trừu tượng ở một vài tài liệu). Một ADT là một kiểu dữ liệu
  * được định nghĩa với một hoặc nhiều hàm tạo[*] - mỗi trong số đó nhận không hoặc nhiều hơn các đối
  * số. Ta nói rằng một kiểu dữ liệu đó là tổng hay hợp của các hàm tạo của nó, và mỗi hàm tạo là tích của
  * các đối số của nó - nguồn gốc cái tên *kiểu dữ liệu đại số*.
  *
  * [*] hàm tạo: nguyên gốc `constructor`, trong wiki thì constructor chỉ được coi là một thứ `giống hàm`,
  * nhưng không thể dịch thuần việt được trừ khi mượn từ hán nôm (khởi tạo giả/khởi tạo tử/toán tử tạo?).
  *
  * Tham khảo thêm:
  * https://en.wikipedia.org/wiki/Algebraic_data_type
  *
  * Example
  * -------
  *
  * `Pair` và `Tuple` của mọi arity[**] là một ví dụ của ADT.
  * scala> val p = ("Bob", 42)
  * p: (java.lang.String, Int) = (Bob,42)
  * scala> p._1
  * res0: java.lang.String = Bob
  * scala> p._2
  * res1: Int = 42
  * scala> p match { case (a,b) => b }
  * res2: Int = 42
  * Trong ví dụ trên, ("Bob", 42) là một `Pair` của (String,Int), tương tự như Tuple2[String,Int]. Chúng ta
  * có thể trích xuất phần tử thứ nhất hay thứ hai của `pair` này, và có thể so sánh mẫu trên `pair` này
  * giống như với các `case class`.
  *
  * [**] arity: số lượng đối số của hàm, đối lượng/tham lượng ?
  *
  */

/**
  * Exercises
  *
  * ADT có thể được dùng để định nghĩa các kiểu dữ liệu khác.  `Pattern matching` cung cấp cho ta một cách
  * dễ dàng để thao tác trên các phần tử của ADT. Chúng ta sẽ thử sau khi định nghĩa ra một kiểu cây nhị
  * phân đơn giản.
  *
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * EXERCISE 3.25
    *
    * Write a function `size` that counts the number of nodes (leaves and branches) in a tree.
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /**
    * EXERCISE 3.26
    *
    * Write a function maximum that returns the maximum element in a `Tree[Int]`. (Note: In Scala, you can use
    * `x.max(y)` or `x max y` to compute the maximum of two integers `x` and `y`.)
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
    * EXERCISE 3.27
    *
    * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    */
  def depth[A](t: Tree[A]) = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  /**
    * EXERCISE 3.28
    *
    * Write a function `map`, analogous to the method of the same name on List, that modi- fies each element in
    * a tree with a given function.
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(_) => Leaf(f(_))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
    * ADTs and encapsulation
    *
    * One might object that algebraic data types violate encapsulation by making public the internal
    * representation of a type. In FP, we approach concerns about encapsulation differently—we don’t typically
    * have delicate mutable state which could lead to bugs or violation of invariants if exposed publicly.
    * Exposing the data constructors of a type is often fine, and the decision to do so is approached much like
    * any other decision about what the public API of a data type should be.15
    *
    * We do typically use ADTs for situations where the set of cases is closed (known to be fixed). For `List` and
    * `Tree`, changing the set of data constructors would significantly change what these data types are. `List`
    * is a singly linked list—that is its nature— and the two cases Nil and Cons form part of its useful public API.
    * We can certainly write code that deals with a more abstract API than List (we’ll see examples of this later
    * in the book), but this sort of information hiding can be handled as a separate layer rather than being baked
    * into List directly.
    *
    * Ai đó có thể thấy khó chịu rằng kiểu dữ liệu đại số vi phạm tính đóng gói bởi nó công bố những mô tả
    * bên trong của kiểu dữ liệu. Thật ra, trong lập trình hàm, chúng ta tiếp cận những vấn đề liên quan đến tính
    * đóng gói theo một cách khác - ta không gặp sự khó chịu trước sự tồn tại của những trạng thái thay đổi được
    * của object - cái mà dẫn đến lỗi hoặc làm xâm hại những phần tử không phải biến (nếu chúng được công khai ra
    * ngoài). Công khai hàm tạo của một kiểu thường ổn, và quyết định như vậy được tiếp cận giống như khi quyết
    * định công khai bất cứ API nào khác của kiểu dữ liệu.
    *
    * Chúng ta thường sử dụng kiểu đại số cho những tình huống mà tập hợp các `case` là đóng (đã biết trước
    * là cố định). Cho `List` và `Tree`, thay đổi tập các hàm tạo có thể làm thay đổi thấy rõ bản thân những
    * kiểu đó. `List` bản thân nó là một danh sách liên kết đơn, và hai `case` `Nil` và `Cons` là một phần của
    * API công khai của nó. Chúng ta hiển nhiên có thể viết ra mã có tính trừu tượng hơn cả `List` (theo dõi
    * trong chương sau) (để giải quyết chuyện đóng gói), nhưng việc ẩn thông tin này nên được xử lý như một
    * lớp riêng biệt thay vì phệt luôn vào `List`.
    */

  /**
    * EXERCISE 3.29
    *
    * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
    * Reimplement them in terms of this more general function. Can you draw an analogy between this fold function
    * and the left and right folds for List?
    */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B) = t match {
    case Leaf(_) => f(_)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]) = fold(t)(_ => 1)((l, r) => 1 + l + r)

  def maximumViaFold(t: Tree[Int]) = fold(t)(_)((l, r) => l max r)

  def depthViaFold[A](t: Tree[A]) = fold(t)(_ => 0)((l, r) => 1 + (l max r))

}
