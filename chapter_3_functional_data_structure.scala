sealed trait Listt[+A]

case object Nill extends Listt[Nothing]
case class ListtCons[+A] (head: A, tail: Listt[A]) extends Listt[A]


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
  * (https://stackoverflow.com/questions/7302206/why-doesnt-scala-have-static-members-inside-a-class)
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
  def apply[A](as: A*): Listt[A] = {
    if (as.isEmpty) Nill
    else ListtCons(as.head, apply(as.tail: _*)) // `:_*` means treat as.tail as `apply`'s argument sequence
  }

  def sum(ints: Listt[Int]): Int = ints match {
    case Nill => 0
    case ListtCons(h: Int, t: Listt[Int]) => h + sum(t) // first time we met partern matching
  }

  def product(ds: Listt[Double]): Double = ds match {
    case Nill => 1.0
    case ListtCons(0.0, _) => 0.0
    case ListtCons(h, t) => h * product(t)
  }

  def fill[A](n: Int, a: A): Listt[A] = {
    if (n <= 0) Nill
    else ListtCons(a, fill(n - 1, a))
  }
}

object App {
  def main(args: Array[String]): Unit = {
    println(Listt.sum(Listt(2, 3, 4, 5)))
  }
}
