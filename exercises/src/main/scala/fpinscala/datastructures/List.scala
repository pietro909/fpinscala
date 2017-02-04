package fpinscala.datastructures

import fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def sumFR(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  /* Can't be short-circuited */
  def productFR(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(head, tail) => Cons(h, tail)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    n match {
      case 0 => l
      case _ => drop(tail(l), n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) if (f(head)) => dropWhile(tail, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, Cons(head2, tail)) => Cons(head, Cons(head2, init(tail)))
      case Cons(head, _) => Nil
    }

  def length[A](l: List[A]): Int =
    foldRight[A, Int](l, 0)((_, n) => n + 1)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(head, tail) => {
        println(s"FR - ${head}, ${tail}")
        f(head, foldRight(tail, z)(f))
      }
    }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(head, tail) => {
        println(s"FL - ${head}, ${tail}")
        foldLeft(tail, f(z, head))(f)
      }
    }

  def fLfilter[A](l: List[A])(f: A => Boolean): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => {
        println(s"  f - $a, $b => ${f(a)}")
        if (f(a)) Cons(a, b) else b
    })

  def fRfilter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, b) => {
        println(s"  f - $a, $b => ${f(a)}")
        if (f(a)) Cons(a, b) else b
    })

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) =>
        println(s"$head, $tail => ${f(head)}")
        if (f(head)) Cons(head, filter(tail)(f))
        else filter(tail)(f)
      case Nil => Nil
    }

  // defined in terms of foldLeft, just swapping the arguments
  def foldRightOptimized[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((x, y) => f(y, x))

  def sumFL(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productFL(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int =
    foldLeft(l, 0)((n, _) => n + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft[A, List[A]](l, Nil)((b, a) => Cons(a, b))

  def appendWithFoldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldRightOptimized(l1, l2)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] = {
    def loop(acc: List[A], rest: List[List[A]]): List[A] =
      rest match {
        case Nil => acc
        case Cons(head, tail) if length(head) == 0 => loop(acc, tail)
        case Cons(head, tail) => loop(append(acc, head), tail)
      }

    loop(Nil, l)
  }

  def plusOne(l: List[Int]): List[Int] =
    l match {
      case Cons(head, tail) => Cons(head + 1, plusOne(tail))
      case Nil => Nil
    }

  def listToString(l: List[Double]): List[String] =
    l match {
      case Cons(head, tail) => Cons(head.toString, listToString(tail))
      case Nil => Nil
    }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    l match {
      case Cons(head, tail) => Cons(f(head), map(tail)(f))
      case Nil => Nil
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Cons(head, tail) =>
        append(f(head), flatMap(tail)(f))
      case Nil => Nil
    }

  def flatFilter[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(e => {
      if (f(e)) List(e)
      else Nil
    })

  def zip(al: List[Int], bl: List[Int]): List[Int] =
    (al, bl) match {
      case (Cons(ha, ta), Cons(hb, tb)) =>
        Cons(ha + hb, zip(ta, tb))
      case _ =>
        Nil
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def loop(acc: List[A], list: List[A], sublist: List[A]): List[A] =
      (list, sublist) match {
        case (Cons(hA, tA), Cons(hB, tB)) =>
          if (hA == hB) loop(Cons(hA, acc), tA, tB)
          else loop(acc, tA, sublist)
        case _ =>
          acc
      }
    length(loop(Nil, sup, sub)) == length(sub)
  }
}
