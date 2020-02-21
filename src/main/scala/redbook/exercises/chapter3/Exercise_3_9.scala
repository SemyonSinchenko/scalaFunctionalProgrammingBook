package redbook.exercises.chapter3

object Exercise_3_9 {
    def length[A](xs: List[A]): Int = List.foldRight(xs, 0)((a, b) => b + 1)

    def main(args: Array[String]) = {
        val xs = List(1, 2, 3, 4, 5, 6);
        println(length(xs))
    }
}