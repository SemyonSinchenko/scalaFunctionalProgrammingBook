package redbook.exercises.chapter3

object Exercise_3_13 {
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        Exercise_3_10.foldLeft(Exercise_3_12.reverse(as), z)(f)
    }

    def main(args: Array[String]) = {
        val xs = List(1, 2, 3, 4, 5, 6)
        println(Exercise_3_10.foldLeft(xs, new StringBuilder(""))((a, b) => b.append(a)).toString);
        println(foldRight(xs, new StringBuilder(""))((a, b) => b.append(a)).toString);
    }
}