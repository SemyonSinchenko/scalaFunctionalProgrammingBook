package redbook.exercises.chapter3

object Exercise_3_15 {
    def concat[A](xs: List[List[A]]): List[A] = {
        Exercise_3_12.reverse(
            Exercise_3_10.foldLeft[List[A], List[A]](xs, Nil)(
                (a, b) => Exercise_3_10.foldLeft[A, List[A]](a, b)((c, d) => Cons(c, d)))
        )
    }

    def main(args: Array[String]) = {
        val xs = List(List(1, 2, 3), List(3, 4, 5), List(6, 7, 8))
        println(concat(xs))
    }
}