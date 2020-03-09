package redbook.exercises.chapter2

import org.scalatest.funsuite.AnyFunSuite

class ExercisesTest extends AnyFunSuite {
  test("test Fibonacci") {
    assert(Exercises.fib(1) == 1)
    assert(Exercises.fib(2) == 1)
    assert(Exercises.fib(3) == 2)
    assert(Exercises.fib(4) == 3)
    assert(Exercises.fib(5) == 5)
    assert(Exercises.fib(6) == 8)
    assert(Exercises.fib(14) == 377)
  }

  test("test isSorted") {
    assert(Exercises.isSorted[Int](Array(1, 2, 3, 4, 5), _ < _))
    assert(!Exercises.isSorted[Double](Array(0.1, -0.9, 14.1, 899, -400), _ < _))
  }

  test("test curry") {
    def curriedAdd = Exercises.curry((a: Int, b: Int) => a + b).apply(3)
    assert(curriedAdd(5) == 8)
  }

  test("test uncurry") {
    def curriedAdd = Exercises.curry((a: Int, b: Int) => a + b)
    def uncurriedAdd = Exercises.uncurry(curriedAdd)
    assert(uncurriedAdd(5, 3) == 8)
  }

  test("compose") {
    def f = (a: Int) => Math.pow(a, 3)
    def g = (a: Int) => a * 2
    def compose = Exercises.compose(f, g)
    assert(compose(2) == 64)
  }
}