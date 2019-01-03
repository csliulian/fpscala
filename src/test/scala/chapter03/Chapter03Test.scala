package chapter03

import chapter03.List._
import org.scalatest.FunSuite

class Chapter03Test extends FunSuite {

  val l = List(1, 2, 3, 4, 5)
  val s = List(6, 7, 8, 9)
  val dl = List(1.0, 2.0, 3.0, 4.0, 5.0)

  test("Exercise 3.1") {
    // Exercise 3.1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(a, Cons(2, Cons(4, _))) => a
      case Nil => 42
      case Cons(a, Cons(b, Cons(3, Cons(4, _)))) => a + b
      case Cons(h, _) => h
      case _ => 101
    }
    assert(x == 3)
  }

  // Exercise 3.2
  test("Exercise 3.2") {
    assert(tail(Nil) == Nil)
    assert(tail(s) == List(7, 8, 9))
    assert(tail(tail(tail(l))) == List(4, 5))
  }

  // Exercise 3.3
  test("Exercise 3.3") {
    assert(setHead(Nil, 5) == Cons(5, Nil))
    assert(setHead(l, 7) == List(7, 2, 3, 4, 5))
  }

  // Exercise 3.4
  test("Exercise 3.4") {
    assert(drop(l, 3) == List(4, 5))
    assert(drop(l, 5) == Nil)
    assert(drop(l, 6) == Nil)
  }

  // Exercise 3.5
  test("Exercise 3.5") {
    assert(dropWhile(l, (x: Int) => x < 4) == List(4, 5))
    assert(dropWhile(l, (x: Int) => x < 100) == Nil)
  }

  // Exercise 3.6
  test("Exercise 3.6") {
    assert(init(l) == List(1, 2, 3, 4))
    assert(init(Nil) == Nil)
    assert(init(List(1)) == Nil)
  }

  // Exercise 3.7
  test("Exercise 3.7") {
    assert(sum2(l) == 15)
    assert(product2(dl) == 120.0)
  }

  // Exercise 3.8
  test("Exercise 3.8") {
    assert(foldRight(l, Nil: List[Int])(Cons(_, _)) == l)
  }

  // Exercise 3.9
  test("Exercise 3.9") {
    assert(length(l) == 5)
    assert(length(s) == 4)
  }

  // Exercise 3.11
  test("Exercise 3.11") {
    assert(sum3(l) == 15)
    assert(product3(dl) == 120.0)
    assert(length3(l) == 5)
  }

  // Exercise 3.12
  test("Exercise 3.12") {
    assert(reverse(Nil) == Nil)
    assert(reverse(l) == List(5, 4, 3, 2, 1))
  }

  // Exercise 3.14
  test("Exercise 3.14") {
    assert(appendUseFoldRight(l, s) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  // Exercise 3.15
  test("Exercise 3.15") {
    assert(concat(List(l, s)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  // Exercise 3.16
  test("Exercise 3.16") {
    assert(addOne(Nil) == Nil)
    assert(addOne(s) == List(7, 8, 9, 10))
  }

  // Exercise 3.17
  test("Exercise 3.17") {
    assert(convertToString(Nil) == Nil)
    assert(convertToString(dl) == List("1.0", "2.0", "3.0", "4.0", "5.0"))
  }

  // Exercise 3.18
  test("Exercise 3.18") {
    assert(map(s)(x => 2 * x - 1) == List(11, 13, 15, 17))
  }

  // Exercise 3.19
  test("Exercise 3.19") {
    assert(filter(Nil: List[Int])(_ => true) == Nil)
    assert(filter(l)(x => x % 2 == 0) == List(2, 4))
  }

  // Exercise 3.20
  test("Exercise 3.20") {
    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  // Exercise 3.21
  test("Exercise 3.21") {
    assert(filterUseFlatMap(Nil: List[Int])(_ => true) == Nil)
    assert(filterUseFlatMap(l)(x => x % 2 == 0) == List(2, 4))
  }

}
