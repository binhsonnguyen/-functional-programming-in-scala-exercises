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

/**
  * EXERCISE 3.25
  *
  * Write a function `size` that counts the number of nodes (leaves and branches) in a tree.
  */
def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => size(l) + size(r) + 1
}
