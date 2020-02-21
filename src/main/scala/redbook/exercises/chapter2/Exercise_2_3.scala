package redbook.exercises.chapter2

object Exercise_2_3 {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
        (a: A) => {
            (b: B) => f(a, b)
        }
    }

    def main(args: Array[String]): Unit = {
        val f = curry[Int, Int, Int]((a: Int, b: Int) => a + b)
        val curried = f(4)

        println(curried(3))
        println(curried(5))
    }
}