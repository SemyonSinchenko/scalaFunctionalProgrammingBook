package redbook.exercises.chapter3

object Exercise_3_12 {
    def reverse[A](xs: List[A]): List[A] = Exercise_3_10.foldLeft[A, List[A]](xs, Nil)(
        ((a, b) => Cons(a, b))
    )

    def main(args: Array[String]) = {
        val xs = List(1, 2, 3, 4, 5, 6)
        println(reverse(xs))
    }
}