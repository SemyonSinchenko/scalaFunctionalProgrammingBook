package redbook.exercises.chapter3

import org.scalatest.funsuite.AnyFunSuite

class ExercisesTest extends AnyFunSuite {
  test("test tail") {
    assert(Nil == List.tail(List(1)))
    assert(Nil == List.tail(Nil))
    assert(List(1, 2, 3) == List.tail(List(500, 1, 2, 3)))
  }

  test("test setHead") {
    assert(List(1, 3, 4) == List.setHead(List(2, 3, 4), 1))
    assert(Nil == List.setHead(Nil, 1))
  }

  test("test drop") {
    assert(List(3, 4, 5) == List.drop(List(1, 2, 3, 4, 5), 2))
    assert(Nil == List.drop(List(1, 2, 3), 3))
    assert(Nil == List.drop(List(1, 2, 3), 999))
  }

  test("test dropWhile") {
    assert(List(1, 2, 3) == List.dropWhile[Int](List(-1, -1, -1, 1, 2, 3), _ < 0))
    assert(Nil == List.dropWhile[Int](List(3, 4, 5), _ > 0))
  }

  test("test init") {
    assert(List(1, 2, 3, 4) == List.init(List(1, 2, 3, 4, 5)))
  }

  test("test length") {
    assert(0 == List.length(Nil))
    assert(4 == List.length(List(1, 2, 3, 4)))
  }

  test("test foldLeft + sum3, prod3, length3") {
    assert(4 == List.sumEffective(List(1, 2, 1)))
    assert(9 == List.productEffective(List(1, 3, 3, 1)))
    assert(4 == List.lengthEffective(List(1, 2, 3, 4)))
  }

  test("test reverse") {
    assert(List(3, 2, 1) == List.reverse(List(1, 2, 3)))
  }

  test("test append") {
    assert(List(1, 2, 3, 4) == List.appendViaFoldLeft(List(1, 2, 3))(4))
    assert(List(1, 2, 3, 4) == List.appendViaFoldRight(List(1, 2, 3))(4))
  }

  test("test concat") {
    assert(List(1, 2, 3, 4, 5, 6) == List.concat(List(List(1, 2, 3), List(4, 5), List(6))))
  }

  test("test addOme") {
    assert(List(2, 3, 4) == List.addOne(List(1, 2, 3)))
  }

  test("test toString") {
    assert("1.0 2.0 3.0" == List.toString(List(1, 2, 3)))
  }

  test("test map") {
    assert(List(1, 2, 3) == List.map(List(1, 2, 3))((a: Int) => a))
    assert(List(2, 4, 6) == List.map(List(1, 2, 3))(_ * 2))
  }

  test("test filter") {
    assert(List(1, 2, 3) == List.filter(List(-1, -2, 1, -4, 2, -5, 3))(_ < 0))
  }

  test("test flatMap") {
    assert(List(1, 1, 2, 2, 3, 3) == List.flatMap(List(1, 2, 3))(i => List(i, i)))
  }

  test("test filter via flatMap") {
    assert(List(1, 2, 3) == List.filterViaFlatMap(List(-1, -2, 1, -4, 2, -5, 3))(_ < 0))
  }
}
