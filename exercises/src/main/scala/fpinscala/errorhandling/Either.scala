package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] =
   this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
   this match {
     case Right(a) => f(a)
     case Left(e) => Left(e)
   }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
   this flatMap  (_ => b)

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   this flatMap { aa => b map { bb => f(aa, bb) } }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case h::t =>
        f(h) flatMap { b => traverse(t)(f) map { bs => b::bs } }
        // the other way round, returns the LAST exception
        // traverse(t)(f).flatMap { bs => f(h) map { b => b::bs } }
      case Nil => Right(List())
    }

  // with for-comprehension
  def fTraverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case h::t =>
        for {
          b <- f(h)
          bs <- traverse(t)(f)
        } yield b::bs
      case Nil => Right(List())
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
     es match {
      case h::t =>
        for {
          b <- h
          bs <- sequence(t)
        } yield b::bs
      case Nil => Right(List())
    }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}