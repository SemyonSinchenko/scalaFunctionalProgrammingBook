package redbook.exercises.chapter3

object Exercise_3_2 {
    def tail[A](xs: List[A]): List[A] = {
        xs match {
            case Nil => Nil
            case Cons(h, t) => t
        }
    }

    def main(args: Array[String]) = {
        val x = List(1, 2, 3, 4, 5, 6)
        println(tail(x))
        println(tail(Nil))
    }
}