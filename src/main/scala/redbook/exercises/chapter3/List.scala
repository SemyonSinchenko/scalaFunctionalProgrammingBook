package redbook.exercises.chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]


object List {
    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(x, xs) => x * product(xs)
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

    def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

    /**
     * Exercise 3.2
     * Implement the function tail for removing the first element of a List.
     * Note that the function takes constant time.
     * What are different choices you could make in your implementation if the List is Nil?
     * We’ll return to this question in the next chapter.
     */
    def tail[A](xs: List[A]): List[A] = {
        xs match {
            case Nil => Nil
            case Cons(_, t) => t
        }
    }

    /**
     * Exercise 3.3
     * Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
     */
    def setHead[A](xs: List[A], newHead: A): List[A] = {
        xs match {
            case Nil => Nil
            case Cons(_, t) => Cons(newHead, t)
        }
    }

    @tailrec
    /**
     * Exercise 3.4
     * Generalize tail to the function drop, which removes the first n elements from a list.
     * Note that this function takes time proportional only to the number of elements being dropped
     * — we don’t need to make a copy of the entire List.
     */
    def drop[A](xs: List[A], n: Int): List[A] = {
        n match {
            case 0 => xs
            case _ => xs match {
                case Nil => Nil
                case Cons(_, t) => drop(t, n - 1)
            }
        }
    }

    @tailrec
    /**
     * Exercise 3.5
     * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
     */
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        l match {
            case Nil => Nil
            case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
        }
    }

    /**
     * Exercise 3.6
     * Not everything works out so nicely.
     * Implement a function, init, that returns a List consisting of all but the last element of a List.
     * So, given List(1,2,3,4), init will return List(1,2,3).
     * Why can’t this function be implemented in constant time like tail?
     */
    def init[A](l: List[A]): List[A] = {
        l match {
            case Nil => Nil
            case Cons(_, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))
        }
    }

    /**
     * Exercise 3.9
     * Compute the length of a list using foldRight.
     */
    def length[A](xs: List[A]): Int = foldRight(xs, 0)((_, b) => b + 1)

    @tailrec
    /**
     * Exercise 3.10
     * Our implementation of foldRight is not tail-recursive and will result in
     * a StackOverflowError for large lists (we say it’s not stack-safe).
     * Convince yourself that this is the case, and then write another general list-recursion function,
     * foldLeft, that is tail-recursive, using the techniques we discussed in the previous chapter.
     */
    def foldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(h, z))(f)
    }

    /**
     * Exercise 3.11
     * Write sum, product, and a function to compute the length of a list using foldLeft.
     */
    def sumEffective(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)
    def productEffective(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)
    def lengthEffective[A](xs: List[A]): Int = foldLeft(xs, 0)((_, b) => b + 1)

    /**
     * Exercise 3.12
     * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
     * See if you can write it using a fold.
     */
    def reverse[A](xs: List[A]): List[A] = foldLeft[A, List[A]](xs, Nil)((a, b) => Cons(a, b))

    /**
     * Exercise 3.13
     * Hard: Can you write foldLeft in terms of foldRight?
     * How about the other way around?
     * Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
     * which means it works even for large lists without overflowing the stack.
     */
    def foldRightTailRec[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        foldLeft(reverse(as), z)(f)
    }

    /**
     * Exercise 3.14
     * Implement append in terms of either foldLeft or foldRight.
     */
    def appendViaFoldLeft[A](xs: List[A])(z: A): List[A] = {
        foldLeft[A, List[A]](reverse(xs), Cons(z, Nil))((a, b) => Cons(a, b))
    }

    def appendViaFoldRight[A](xs: List[A])(z: A): List[A] = {
        foldRightTailRec(xs, Cons(z, Nil))((a, b) => Cons(a, b))
    }

    /**
     * Exercise 3.15
     * Hard: Write a function that concatenates a list of lists into a single list.
     * Its runtime should be linear in the total length of all lists.
     * Try to use functions we have already defined.
     */
    def concat[A](xs: List[List[A]]): List[A] = {
        reverse(
            foldLeft[List[A], List[A]](xs, Nil)(
                (a, b) => foldLeft[A, List[A]](a, b)((c, d) => Cons(c, d)))
        )
    }

    /**
     * Exercise 3.16
     * Write a function that transforms a list of integers by adding 1 to each element.
     * (Reminder: this should be a pure function that returns a new List!)
     */
    def addOne(xs: List[Int]): List[Int] = {
        foldRightTailRec[Int, List[Int]](xs, Nil)((a, b) => Cons(a + 1, b))
    }

    /**
     * Exercise 3.17
     * Write a function that turns each value in a List[Double] into a String.
     * You can use the expression d.toString to convert some d: Double to a String.
     */
    def toString(xs: List[Double]): String = {
        @scala.annotation.tailrec
        def toString(xs: List[Double], buffer: StringBuilder): String = xs match {
            case Nil => buffer.dropRight(1).toString()
            case Cons(h, t) => toString(t, buffer.append(h.toString).append(" "))
        }

        toString(xs, new StringBuilder(""))
    }

    /**
     * Exercise 3.18
     * Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.
     */
    def map[A, B](as: List[A])(f: A => B): List[B] = {
        foldRightTailRec[A, List[B]](as, Nil)((a, b) => Cons(f(a), b))
    }

    /**
     * Exercise 3.19
     * Write a function filter that removes elements from a list unless they satisfy a given predicate.
     * Use it to remove all odd numbers from a List[Int].
     */
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
        foldRightTailRec[A, List[A]](as, Nil)((a, b) => {
            if (f(a)) b else Cons(a, b)
        })
    }

    /**
     * Exercise 3.20
     * Write a function flatMap that works like map except that the function given will
     * return a list instead of a single result, and that list should be inserted into the final resulting list.
     */
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
        concat(map(as)(f))
    }

    /**
     * Exercise 3.21
     * Use flatMap to implement filter.
     */
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
        flatMap(as)((a: A) => if (f(a)) Nil else List(a))
    }
}