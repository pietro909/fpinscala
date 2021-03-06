package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Cons(h, t) =>
        h() :: t().toList
      case Empty =>
        List()
    }


  // ok, it's wrong. It's a scanLeft, actually -_-
  def scanRightWrong[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    type S = (B, Stream[A])
    Stream.unfold[B, S]((z, this))(s => {
      val (result, current) = s
      current match {
        case Cons(h, t) =>
          val intermediate: B = f(h(), result)
          println(s"${h()}, ${result}")
          Some(intermediate, (intermediate, t()))
        case Empty =>
          None
      }
    })
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    type R = Stream[B]
    foldRight[R](Stream(z))((a, streamB) => {
      streamB match {
        case Cons(h, t) =>
          val result: B = f(a, h())
          cons(result, streamB)
        case _ => empty
      }
    })
  }

  def tailsWithScanR: Stream[Stream[A]] =
    scanRight[Stream[A]](empty)((a, b) => cons(a, b))

  /*
   * Is wrong, issue open on https://github.com/fpinscala/fpinscala/issues/504
   */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def pietroTails: Stream[Stream[A]] = {
    val tail = Stream.unfold(this) {
      case Cons(h, t) =>
        Some(t(), t())
      case Empty =>
        None
    }
    Stream.cons(this, tail)
  }

   def verboseTails: Stream[Stream[A]] = {
    type R = Stream[A]
    type S = (R, R)
    val tail = Stream.unfold[R, S]((Stream[A](), this))(s => {
      val (_, current) = s
      current match {
        case Cons(h, t) =>
          lazy val tail = t()
          Some(tail, (tail, tail))
        case Empty =>
          None
      }
    })
    Stream.cons(this, tail)
  }
  
  // check answer 7
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => {
      println(s"  f - $a, ${b.toList} => ${p(a)}")
      //if (p(a)) Cons(() => a, () => b) else b
      if (p(a)) cons(a, b) else b
    })

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) =>
        println(s"FR - ${h()}, ${t().toList}")
        val evTail = t().foldRight(z)(f)
        println(s"FR - evTail => ${evTail}")
        f(h(), evTail) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
      case _ => Empty
    }

  def unfTake(n: Int): Stream[A] =
    Stream.unfold[A, (Stream[A], Int)]((this, 0)) {
        case (Cons(head, tail), index) if index < n =>
          Some(head(), (tail(), index+1))
        case _ =>
          None
    }

  def unfTakeWhile(f: A => Boolean): Stream[A] =
    Stream.unfold[A, (Stream[A], Int)]((this, 0)) {
        case (Cons(head, tail), index) if f(head()) =>
          Some(head(), (tail(), index+1))
        case _ =>
          None
    }

  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold[C, (Stream[A], Stream[B])](this, stream) {
      case (Cons(hA, tA), Cons(hB, tB)) =>
       Some(f(hA(), hB()), (tA(), tB()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, s2)) {
      case (Cons(hA, tA), Cons(hB, tB)) => Some(((Some(hA()), Some(hB())), (tA(), tB())))
      case (Cons(hA, tA), Empty) => Some(((Some(hA()), None), (tA(), Stream())))
      case (Empty, Cons(hB, tB)) => Some(((None, Some(hB())), (Stream(), tB())))
      case _ => None
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n-1)
      case _ => Empty
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

  def foldTakeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (p(a)) Cons(() => a, () => b.takeWhile(p)) else Stream())

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((a,b) => Cons(() => f(a), () => b))

  def unfMap[B](f: A => B): Stream[B] =
    Stream.unfold[B, Stream[A]](this) {
      case Cons(head, tail) => Some((f(head()), tail()))
      case Empty => None
    }

  def answerFilter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => {
      println(s"$h, $t => ${f(h)}")
      if (f(h)) cons(h, t)
      else t
    })

  def correctFilter(p: A => Boolean): Stream[A] =
  // check answer 7
    foldRight(empty[A])((a, b) => {
      println(s"$a, $b => ${p(a)}")
      if (p(a)) cons(a, b) else b
    })


  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Stream[B]())((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean =
    zipWith(s)((a, b) => a == b).foldRight(true)(_ && _)



  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  val unfOnes: Stream[Int] = unfold(1)(n => Some(n, n))

  def constant[A](n: A): Stream[A] = cons(n, constant(n))
  def unfConstant[A](n: A): Stream[A] = unfold(n)(n => Some(n, n))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def unfFrom(n: Int): Stream[Int] = unfold(n)(s => Some(s+1, s+1))

  def fib: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = cons(a+b, loop(b, a+b))
    loop(0, 1)
  }

  def unfFib: Stream[Int] =
    unfold[Int, (Int, Int)]((0, 1))(s => {
      val number = s._1 + s._2
      Some((number, (s._2, number)))
    })

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
}
