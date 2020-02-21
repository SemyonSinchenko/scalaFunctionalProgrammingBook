package redbook.exercises.chapter3

object Exercise_3_6 {
    def init[A](l: List[A]): List[A] = {
        l match {
            case Nil => Nil
            case Cons(h, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))
        }
    }

    def main(args: Array[String]) = {
        println(init(List(1, 2, 3, 4, 5, 6, -1)))
    }
}