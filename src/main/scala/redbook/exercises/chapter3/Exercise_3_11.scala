package redbook.exercises.chapter3

object Exercise_3_11 {
    def sum(xs: List[Int]): Int = Exercise_3_10.foldLeft(xs, 0)(_ + _)
    def product(xs: List[Double]): Double = Exercise_3_10.foldLeft(xs, 1.0)(_ * _)
    def length[A](xs: List[A]): Int = Exercise_3_10.foldLeft(xs, 0)((a, b) => b + 1)

    def main(args: Array[String]) = {
        val xs = List(1, 2, 3, 4, 5, 6)
        val xsd = List(1.0, 2.0, 3.0, 4.0)

        println(sum(xs))
        println(product(xsd))
        println(length(xs))
        println(length(xsd))
    }
}