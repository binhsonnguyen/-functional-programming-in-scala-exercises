/* 
Write a recursive function to get the nth Fibonacci number
(http://mng.bz/C29s). The first two Fibonacci numbers are 0 
and 1. The nth number is always the sum of the previous two—the 
sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a 
local tail-recursive function.
*/

object MyExercise {
    def fib(n: Int): Int = {
        @annotation.tailrec
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