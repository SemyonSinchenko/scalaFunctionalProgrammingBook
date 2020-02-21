package redbook.exercises.chapter3

object Exercise_3_5 {
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        l match {
            case Nil => Nil
            case Cons(h, t) => {
                if (f(h)) dropWhile(t, f)
                else l
            }
        }
    }

    def main(args: Array[String]) = {
        val l = List(-1, -3, -9, -4, 3, 5, 7)
        println(dropWhile(l, (x: Int) => x < 0))
    }
}