package redbook.exercises.chapter2

object Exercise_2_4 {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
        (a: A, b: B) => f(a).apply(b)
    }

    def main(args: Array[String]): Unit = {
        val curriedSum = Exercise_2_3.curry((a: Int, b: Int) => a + b)

        println(s"Uncurry result: ${uncurry(curriedSum).apply(2, 3)}")
        println(s"Pure result: ${2 + 3}")
    }
}