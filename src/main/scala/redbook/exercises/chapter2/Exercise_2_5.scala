package redbook.exercises.chapter2

object Exercise_2_5 {
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        (a: A) => f(g(a))
    }

    def main(args: Array[String]) {
        val g = (a: Int) => a.toString
        val f = (a: String) => Integer.parseInt(a)
        val com = compose(f, g)

        println(com(5))
    }
}