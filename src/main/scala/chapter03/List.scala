package chapter03

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // Listing 3.2
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Exercise 3.7
  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // Exercise 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) => 1 + y)

  // Exercise 3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((xs, x) => Cons(x, xs))
  }

  // Exercise 3.13
  def foldLeftUseFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x, xs) => f(xs, x))

  def foldRightUseFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((xs, x) => f(x, xs))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // Exercise 3.14
  def appendUseFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, xs) => Cons(x, xs))
  }

  //Exercise 3.15
  def concat[A](lists: List[List[A]]): List[A] = {
    foldRight(lists, Nil: List[A])((x, y) => appendUseFoldRight(x, y))
  }

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((x, xs) => Cons(x + 1, xs))
  }

  // Exercise 3.17
  def convertToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString, xs))
  }

  // Exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))
  }

  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((x, xs) =>
      if (f(x)) Cons(x, xs)
      else xs
    )
  }

  // Exercise 3.21
  def filterUseFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(x => {
      if (f(x)) List(x)
      else Nil
    })
  }

  // Exercise 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldLeft(l, Nil: List[B])((xs, x) => append(xs, f(x)))
  }

}
