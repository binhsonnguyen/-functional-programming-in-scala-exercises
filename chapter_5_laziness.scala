/*
 * Ở chương 3 chúng ta đã nói về những cấu trúc dữ liệu của lập trình thuần hàm, và sử dụng những
 * danh sách liên kết đơn. Ta đã bao quát một số lượng lớn những phép toán dựa trên list như map,
 * filter, foldLeft, foldRight, zipWith, etc. Ta đã rút ra được là mỗi một phép toán đó duyệt qua
 * hết danh sách đầu vào và xây dựng ra một danh sách mới làm kết quả.
 *
 * Đặt tình huống bạn có một bộ bài và bạn cần lấy đi tất cả những quân lẻ, và lật ngược những
 * quân Q. Dĩ nhiên bạn sẽ lướt qua cả bộ bài, tìm những quân Q và loại những quân lẻ cùng một
 * lúc. Nó tự nhiên, và hiệu quả hơn so với đầu tiên loại những quân lẻ, và sau đó lật những quân
 * Q. Trường hợp thứ hai chính là thứ mà trong scala sẽ được code ra đại loại như thế này:
 *
 * scala> List(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)
 *      List(36,42)
 *
 * Làm thế nào để biến nó thành loại thứ nhất? Câu trả lời chính là khái niệm `non-strictness`,
 * hay `laziness`. Trong chương này chúng ta sẽ tìm hiểu chính xác ý nghĩa của khái niệm đó. Làm
 * việc với một vài triển khai của nó. Để thấy rằng nó thật sự tăng tính hiệu quả và tính module
 * của chương trình.
 *
 * Strict và Non-Strict
 * -
 *
 * Non-strict ám chỉ việc hàm không quan tâm - không định lượng một hoặc nhiều trong số những tham
 * số của nó. Ngược lại, hàm strict sẽ cố định lượng tất cả các tham số của nó. Hàm strict là
 * tiêu chí của hầu hết các ngôn ngữ - thực tế là hầu hết các ngôn ngữ chỉ hỗ trợ hàm có tất cả
 * các tham số đều đã được định lượng hoàn chỉnh. Với scala, trừ khi ta cố gắng làm khác đi, nếu
 * không thì tất cả các hàm sẽ là strict. Như ví dụ sau
 *
 * def square(x: Int): Int = x * x
 *
 * Nếu bạn gọi hàm trên bằng `square(1 + 1)`, thì hàm trên sẽ chỉ biết rằng nó cần tính dựa trên
 * số 2, vì tham số của nó đã được định lượng hoàn toàn từ trước khi truyền vào cho nó rồi. Và
 * nếu gọi bằng `square(sys.ererror"kool!"))`, thì ta sẽ gặp lỗi "kool" trước khi square kịp thực
 * hiện bất cứ thứ gì.
 *
 * Ơ mặc dù bạn có thể chưa biết cú pháp để chỉ ra một non-strict, và có thể bạn không tin, nhưng
 * bạn có thể đã dùng rất nhiều non-strict mà không biết. Phép `&&` cùng với `||` có trong rất
 * nhiều ngôn ngữ trong đó có Scala, là nôn-strict. Thật vậy, `false && sys.error("kool!")` sẽ
 * ngay lập tức cho ra kết quả `false` trong khi phần còn lại của phép toán còn chưa được tính
 * đến. Tương tự như `true || somethingElse()`. Điều tương tự cũng xảy ra với phép toán `ternary`
 * hay phép cấu trúc điều khiển `if`.
 *
 * Đấy chính là những minh chứng về việc non-strict thật sự rất kool. Sau đây ta thử đi vào một
 * tình huống khi mà việc biến hàm strict thành non strict phát huy hiệu quả.
 *
 * scala> def maybeTwice(maybe: Boolean, x: => Int) = if (maybe) x + x else 0
 * maybeTwice: (maybe: Boolean, x: => Int)Int
 *
 * scala> def somethingKool(): Int = { println("kool"); 1 + 1}
 * somethingKool: ()Int
 *
 * scala> maybeTwice(true, somethingKool )
 * kool
 * kool
 * res: Int = 4
 *
 * Biểu thức `1 + 1` đã được tính tới hai lần mà không vì cái gì cả. Ta có thể trì hoãn việc tính
 * biểu thức đó, cho đến khi chương trình của chúng ta thực sự cần, bằng cách sử dụng từ khoá lazy
 * trên một biến tạm tường minh:
 *
 * scala> def maybeTwice(maybe: Boolean, x: => Int) = {
 *   lazy val y = x
 *   if (maybe) y + y else 0
 * }
 * maybeTwice: (maybe: Boolean, x: => Int)Int
 *
 * scala> maybeTwice(true, somethingKool )
 * kool
 * res: Int = 4
 *
 * scala> maybeTwice(false, somethingKool )
 * res: Int = 0
 *
 * Để ý rằng, trong trường hợp maybe là false, somethingKool đã không được chạy, từ khoá lazy đã
 * trì hoãn việc đó và cuối cùng bỏ qua.
 *
 * ĐỊNH NGHĨA CHÍNH XÁC VỀ NGHIÊM NGẶT
 *
 * "Nếu quá trình đánh giá một biểu thức chạy mãi hoặc ném ra một lỗi thay vì trả về một giá trị
 * xác định, ta gọi biểu thức đó là biểu thức không tắt, hoặc biểu thức chạm đáy. Một
 * hàm `f` là nghiêm ngặt nếu nếu biểu thức `f(x)` chạm đáy với mọi `x` chạm đáy.
 *
 * Như vậy thì sử dụng lazy sẽ ngay lập tức phá huỷ tính strict của hàm (nếu có), ta còn nói rằng,
 * hàm non-strict lấy đối số của nó bằng tên thay vì bằng giá trị.
 *
 *
 */


