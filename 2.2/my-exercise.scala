/*
Implement isSorted, which checks whether an Array[A] is sorted according 
to a given comparison function
*/

object MyExercise {
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
        def intComparator(n1: Int, n2: Int): Boolean = {
            n1 <= n2
        }

        val as = Array(-1, 2, 3)
        println(isSorted(as, intComparator))
    }
}