package redbook.exercises.chapter3

object Exercise_3_4 {
    def drop[A](xs: List[A], n: Int): List[A] = {
        n match {
            case 0 => xs
            case _ => xs match {
                case Nil => Nil
                case Cons(h, t) => drop(t, n - 1)
            }
        }
    }

    def main(args: Array[String]) = {
        val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
        println(drop(xs, 1))
        println(drop(xs, 5))
    }
}