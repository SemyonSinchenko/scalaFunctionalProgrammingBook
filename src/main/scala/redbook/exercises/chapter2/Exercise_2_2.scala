package redbook.exercises.chapter2

object Exercise_2_2 {
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        (0 until (as.length - 1))
            .map(f => ordered(as(f), as(f + 1)))
            .reduce(_ && _)
    }

    def main(args: Array[String]): Unit = {
        if (isSorted(Array(1, 3, 4, 7, 9), (a: Int, b: Int) => a < b)) println("Success")
        if (isSorted(Array(0.1, 0.3, 0.9, 1.5), (a: Double, b: Double) => a < b)) println("Success")
    }
}
