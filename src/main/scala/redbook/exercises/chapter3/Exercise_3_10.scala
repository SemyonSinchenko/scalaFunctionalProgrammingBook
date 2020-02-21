package redbook.exercises.chapter3

object Exercise_3_10 {
    def foldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(h, z))(f)
    }
}