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