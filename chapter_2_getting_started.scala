/*
EXERCISE 2.1
Write a recursive function to get the nth Fibonacci number
(http://mng.bz/C29s). The first two Fibonacci numbers are 0 
and 1. The nth number is always the sum of the previous two—the 
sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a 
local tail-recursive function.
*/

object Fibonacci {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, p1: Int, p2: Int): Int = {
            if (n <= 0) p1
            else go(n - 1, p1 + p2, p1)
        }
        go(n, 1, 0)
    }

    def main(args: Array[String]): Unit = {
        println(fib(0) == 1)
        println(fib(1) == 1)
        println(fib(2) == 2)
        println(fib(3) == 3)
        println(fib(4) == 5)
        println(fib(5) == 8)
        println(fib(6) == 13)
    }
}


/*
EXERCISE 2.2
Implement `isSorted`, which checks whether an `Array[A]` is sorted according 
to a given comparison function
*/

object ArrayAnalysis {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def iterator(n: Int): Boolean = {
            if (n >= as.length - 1) true
            else if (! ordered(as(n), as(n + 1))) false
            else iterator(n + 1)
        }
        iterator(0)
    }

    def main(args: Array[String]): Unit = {
        val numbeComparator = (n1: Int, n2: Int) => n1 <= n2
        println(isSorted(Array(), numbeComparator))
        println(isSorted(Array(-1), numbeComparator))
        println(isSorted(Array(-1, 2), numbeComparator))
        println(isSorted(Array(-1, 2, 3), numbeComparator))
        println(!isSorted(Array(2, 1), numbeComparator))
        println(!isSorted(Array(1, 3, 2), numbeComparator))


        val stringComparator = (s1: String, s2: String) => {
            if (s1 == null && s2 == null) true
            else if (s1 == null) true
            else if (s2 == null) false
            else s1.length <= s2.length
        }
        println(isSorted(Array(""), stringComparator))
        println(isSorted(Array("", " "), stringComparator))
        println(!isSorted(Array(" ", ""), stringComparator))
        println(!isSorted(Array(" ", null), stringComparator))
        println(isSorted(Array(null, ""), stringComparator))
    }
}


/*
EXERCISE 2.3
Implement `curry` function
*/
def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a, b)

/*
EXERCISE 2.4
Implement `uncurry` function
*/
def uncurry[A,B,C](f: A => B => C): (A, B) => C => (a, b) => f(a)(b)


/*
EXERCISE 2.5
Implement the higher-order function that composes two functions.
*/
def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
def andThen[A,B,C](f: B => C, g: A => B): A => C = a => compose(f, g)