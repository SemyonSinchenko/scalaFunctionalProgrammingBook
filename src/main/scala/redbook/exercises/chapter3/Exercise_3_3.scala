package redbook.exercises.chapter3

object Exercise_3_3 {
    def setHead[A](xs: List[A], newHead: A): List[A] = {
        xs match {
            case Nil => Nil
            case Cons(h, t) => Cons(newHead, t)
        }
    }

    def main(args: Array[String]) = {
        val x = List(1, 2, 3, 4, 5)
        println(setHead(x, 99))
    }
}