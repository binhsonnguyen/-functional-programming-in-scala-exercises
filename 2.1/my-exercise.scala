object MyExercise {
    def fib(n: Int): Int = {
        def go(n: Int, p1: Int, p2: Int): Int = {
            if (n <= 0) p1
            else go(n - 1, p1 + p2, p1)
        }
        go(n, 1, 0)
    }

    def main(args: Array[String]): Unit = {
        val input = Integer.parseInt(args(0))
        println(fib(input))
    }
}