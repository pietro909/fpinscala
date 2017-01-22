package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _}
// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(value) => Some(f(value))
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case Some(value) => value
      case None => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    flatMap(_ => ob)

  def filter(f: A => Boolean): Option[A] =
    if (map(f) getOrElse false) this else None
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // math.pow(x - m, 2)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m => mean(xs map { x => math.pow(x - m, 2) }) }

  // with for comprehension
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // with pattern matching
  def pmMap2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(va), Some(vb)) => Some(f(va, vb))
      case _ => None
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case head::tail =>
        head flatMap { v => sequence(tail).map(vt => v :: vt) }
      case _ => Some(List())
    }

  // implemented with traverse
  def tSequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(a => a)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = //Option[List[B]] =
    a match {
      case head::tail =>
        f(head).flatMap { b =>
          traverse(tail)(f) map (t => b :: t)
        }
      case _ => Some(List())
    }

  def fTraverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = //Option[List[B]] =
    a match {
      case head::tail =>
        for {
          b <- f(head)
          t <- traverse(tail)(f)
        } yield b :: t
      case _ => Some(List())
    }

}