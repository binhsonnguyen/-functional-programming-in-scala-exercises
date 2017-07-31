/**
  * Nếu throw exception là một hiệu ứng lề - nghĩa là nó không được sử dụng trong lập trình hàm, vậy phải
  * DÙNG GÌ ĐỂ THAY THẾ? Ý tưởng là, chúng ta có thể biểu diễn lại các ngoại lệ bằng các giá trị thông
  * thường, đồng thời ta cũng có thể viết các hàm cấp cao hơn để trừu tượng hoá những mẫu thiết kế thường
  * gặp cho xử lý ngoại lệ. Việc trả về lỗi như những giá trị, là an toàn, và bảo trì được tham chiếu minh
  * bạch, và thông qua việc sử dụng hàm cấp cao, ta giữ được lợi ích cốt lõi của ngoại lệ: sự thống nhất
  * của logic xử lý lỗi.
  *
  * Quay lại làm rõ hơn vấn đề "nguyên nhân có chương này": tại sao ngoại lệ là hiệu ứng lề, hay, tại sao
  * ngoại lệ phá hỏng tham chiếu minh bạch ? Ta sẽ điều tra thông qua một ví dụ, một hàm có tung ngoại lệ.
  */
def faillingFn(i: Int): Int = {
  val y: Int = throw new Exception("fail!")
  try {
    val x = 42 + 5
    x + y
  } catch {
    case e: Exception => 43
  }
}

/**
  * nếu `y` là tham chiếu minh bạch (RT) thì nó phải luôn có thể thay thế được bởi một gía trị cụ thể nào
  * đó. Nhưng đoạn code trên đã chết ngắc ngay từ khi phép gán cho y được hoàn thành. Để vượt qua, ta làm
  * như sau:
  */
def faillingFnImproved(i: Int): Int = {
  try {
    val x = 42 + 5
    x + (throw new Exception("fail!")): Int
  } catch {
    case _: Exception => 43
  }
}
/**
  * Nhớ lại định nghĩa về RT. Định nghĩa đó không bị ảnh hưởng bởi ngữ cảnh, nói cách khác kết quả của
  * một RT hoàn toàn có thể suy luận được mà không cần bất kỳ yếu tố bên ngoài nào. Ví dụ như kết quả của
  * `42 + 5` là không thể bị can thiệp. Ô nếu như vậy thì `throw new Exception("fail!")` là một biểu thức
  * quá mức phụ thuộc ngữ cảnh - nó mang những ý nghĩa khác nhau hoàn toàn phụ thuộc vào khối `try` mà nó
  * nằm trong (nếu có) làm gì.
  *
  * Ta nhìn thấy hai vấn đề của `Exception`:
  *
  * - Nó phá huỷ tính RT và gây ra sự phụ thuộc ngữ cảnh. Cản trở chúng ta sử dụng phép thay thế mẫu.
  * Tạo điều kiện cho những đoạn code bắt exception khó điều khiển và khó hiểu. Đây chính là nguồn gốc
  * của lời khuyên dân gian (chữ của tác giả) rằng `Exception` chỉ được dùng để điều khiển lỗi, không phải
  * để xử lý luồng nghiệp vụ.
  *
  * - `Exception` không có an toàn kiểu (ít nhất là trong ngữ cảnh của Scala, với các ngôn ngữ khác thì
  * tuỳ mà chúng ta có thể bỏ vấn đề này đi, và thay vấn đề khác của ngôn ngữ đó vào).
  *
  * Ngôn ngữ `Java` ép mọi `exception` phải được handle ở đâu đó, hoặc cuối cùng sẽ sinh ra lỗi ở log.
  * Nhưng trò đó không áp dụng được cho HOFs - vốn có tính tổng quát rất cao - và do đó không thể chấp
  * nhận nhiều phiên bản HOFs khác nhau cho mỗi thể loại exception mà hàm đối số của nó tung ra.
  *
  * Dĩ nhiên chúng ta không muốn gặp phải những vấn đề đó, nhưng đổi lại không thể bỏ qua những gì mà
  * exception đã mang lại: củng cố và tập trung hoá logic xử lý lỗi thay vì phân tán nó rải rác. Có một
  * kỹ thuật, dựa trên một ý tưởng xưa cũ: thay vì tung ngoại lệ, chúng ta trả về một giá trị cụ thể
  * ngụ ý rằng đã có lỗi xảy ra. Tổng quát hoá ý tưởng đó, chúng ta tạo ra một kiểu dữ liệu cho "những
  * giá trị có thể thu được", và sử dụng HOF để để túm gói những pattern xử lý lỗi thường gặp. Nó không
  * nông dân như kiểu mã CODE lỗi, mà phải thật kool với hoàn toàn an toàn kiểu, thuần hàm, và đọc không
  * lạ lẫm.
  *
  * Trước hết, thử xem xét một ví dụ thực tế mà ta có thể dùng `exception`.
  */
def mean(xs: Seq[Double]): Double =
  if (xs.isEmpty)
    throw new ArithmeticException("mean of empty list!")
  else xs.sum / xs.length

/**
  * Vầng một ví dụ điển hình của thể loại **hàm thiên vị** - những hàm không hoạt động được với một vài
  * đầu vào đặc thù.
  *
  * Nếu không bắt exception, hàm trên sẽ sinh ra phép tính `0.0 / 0.0` và trả về `Double.NaN`, trong một
  * vài điều kiện thực thế khác, chúng ta có thể trả về `null` - đây là cách phổ biển để không cần dùng
  * tới exception. Nhưng ta avoid nó vì một vài quan điểm sau:
  *
  * - Việc lỗi xảy ra đã hoàn toàn bị giấu đi và compiler không phát hiện ra.
  * - Chỉ cần một chỗ return null cũng có thể tạo ra một số lượng rất lớn những dòng code `if ...` thô
  * thiển.
  * - Không thể đa hình được, với một vài loại `kiểu` và một vài , thậm chí còn không tồn tại một giá hàm
  * trị nào để chúng ta dùng thay thế cho lỗi. Ví dụ `def max[A](xs: Seq[A])(greater: (A,A) => Boolean): A`.
  * - Nó tạo ra một ràng buộc đặc biệt, gây lạ lẫm, khi sử dụng hàm `mean`. Cực kỳ cản trở việc sử dụng
  * `mean` cho HOFs.
  *
  * So, we thử một cách tránh exception khác.
  */
def mean2(xs: Seq[Double], onEmpty: Double): Double = if (xs.isEmpty) onEmpty else xs.sum / xs.length

/**
  * trò mèo trên biến mean thành hàm thuần. Nhưng khốn nạn là nó bắt người dùng `mean` phải ngay lập tức
  * biết cách xử lý những trường hợp lỗi và chỉ định nó trả về một số thực xác định. Ý ở đây là, lỗi
  * đã bị giấu triệt để. Vì nó đã được xử lý luôn rồi. Thử nghĩ về trường hợp `mean` là một toán tử
  * trong một phép tính lớn và yêu cầu tắt luôn, hoặc tính theo kiểu khác hoàn toàn nếu `mean` có lỗi?
  * Thậm chí kiểu "thuần hàm" này còn tệ hơn cả exception.
  */

/**
  * Phương án là, làm rõ trắng đen luôn từ kiểu trả về của hàm, rằng hàm có thể không phải luôn trả về
  * giá trị - mà rõ ràng nó vốn là thế. Bằng cách đó chúng ta có thể giao toàn quyền cho người gọi `mean`
  * cách xử lý vấn đề đó. Ta định nghĩa ra một kiểu mới, `MayBe`.
  */
sealed trait MayBe[+A]
case class Some[+A](get: A) extends MayBe[A]
case object None extends MayBe[Nothing]

/**
  * `mean` bây giờ vẫn thuần hàm, vô cùng R (trong RT), và người gọi mean có toàn quyền quyết định về
  * những case gặp phải. Hàm thiên vị nhan nhản trong code của chúng ta, và đây là cách ta chơi với
  */
def mean3(xs: Seq[Double]): MayBe[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
