package redbook.exercises.chapter3

object Exercise_3_14 {
    def appendViaFoldLeft[A](xs: List[A])(z: A): List[A] = {
        Exercise_3_10.foldLeft[A, List[A]](Exercise_3_12.reverse(xs), Cons(z, Nil))((a, b) => Cons(a, b))
    }

    def appendViaFoldRight[A](xs: List[A])(z: A): List[A] = {
        Exercise_3_13.foldRight(xs, Cons(z, Nil))((a, b) => Cons(a, b))
    }

    def main(args: Array[String]) = {
        val x = List(1, 2, 3, 4, 5)
        println(appendViaFoldLeft(x)(3))
        println(appendViaFoldRight(x)(3))
    }
}