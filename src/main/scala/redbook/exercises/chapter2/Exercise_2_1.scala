package redbook.exercises.chapter2

object Exercise_2_1 {
  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(prev: Int, current: Int, n: Int): Int = {
      if (n <= 2) current
      else go(current, current + prev, n - 1)
    }

    n match {
      case 1 => 0
      case 2 => 1
      case _ => go(0, 1, n)
    }
  }

  def main(args: Array[String]): Unit = {
    (1 to 25).foreach(i => println(s"Fibonacci($i) = ${fib(i)}"))
  }
}
