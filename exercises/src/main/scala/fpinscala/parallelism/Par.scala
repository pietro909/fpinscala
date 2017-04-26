package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

import java.util.concurrent.ExecutorService

/*
Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
We could prevent this using synchronization, but it isn't needed for our purposes here
(also, repeated evaluation of pure values won't affect results).
*/
case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                             f: (A,B) => C) extends Future[C] {
  @volatile var cache: Option[C] = None
  def isDone = cache.isDefined
  def isCancelled = a.isCancelled || b.isCancelled
  def cancel(evenIfRunning: Boolean) =
    a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
  def get = compute(Long.MaxValue)
  def get(timeout: Long, units: TimeUnit): C =
    compute(TimeUnit.NANOSECONDS.convert(timeout, units))

  private def compute(timeoutInNanos: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.nanoTime
      val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime;
      val aTime = stop-start
      val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }
}

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
//      val af = a(es)
//      val bf = b(es)
      // UnitFuture(f(af.get, bf.get))
      Map2Future(a(es), b(es), f)
      // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures.
      // This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait.
      // It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures
      // `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`.
      // In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time
      // spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] =
    {
      def f1(a: A, b: B)(c: C) = f(a,b,c)
      val f2 = map2(a,b)(f1)
      val pd: Par[D] = map2(c, f2)((c, f2) => f2(c))
      pd
    }

  /*def map4[A,B,C,D,E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A,B,C,D) => E): Par[E] =
    {
      def f1(a: A, b: B)(c: C)(d: D) = f(a,b,c,d)
      val f2: Par[(C => D => E)] = map2(pa,pb)(f1)
      val f4: Par[(D => E)] = map2(pc, f2)((c, f3) => f3(c))
      val pd: Par[E] = map2(pd, f4)((d: D, f5: (D => E)) => f5(d))
      pd
    }*/

  def asyncF[A,B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))


  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence[B](fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](lazyUnit(List[A]()))(
      (pa: Par[A], acc: Par[List[A]]) =>
        map2[A, List[A], List[A]](pa, acc)((a: A, acc: List[A]) => a::acc)
    )

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val af = asyncF((a: A) => {
      if (f(a)) List(a)
      else List()
    })
    val lpla: List[Par[List[A]]] = as.map(a => af(a))
    val plla: Par[List[List[A]]] = sequence(lpla)
    val pla: Par[List[A]] = map(plla)(_.flatten)
    pla
  }


  def sum(ints: IndexedSeq[Int]): Par[Int] =
    generalSum(ints, 0)((a,b) => a+b)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    generalSum(ints, 0)((a,b) => if (a > b) a else b)

  def paragraphLength(paragraphs: List[String]): Par[Int] =
    map(
      generalSum[String](paragraphs.toIndexedSeq, "")(_++_)
    )(_.length)

  def generalSum[A](ints: IndexedSeq[A], neutralElement: A)(f: (A, A) => A): Par[A] =
    fork {
      if (ints.size <= 1) {
        unit(ints.headOption.getOrElse(neutralElement))
      } else {
        val (l, r) = ints.splitAt(ints.length / 2)
        map2(
          generalSum(l, neutralElement)(f),
          generalSum(r, neutralElement)(f)
        )(f(_, _))
      }
    }


  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]])(f: A => IndexedSeq[A]): Par[IndexedSeq[A]] =
    fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(f) // a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l)(f), sequenceBalanced(r)(f))(_ ++ _)
      }
    }

  def sequenceWithBalanced[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq)(a => Vector(a)))(_.toList)


  /** This is like using Par.run, actually. **/
  def sequenceWrong[A](ps: List[Par[A]]): Par[List[A]] =
    (e: ExecutorService) => {
      val la: List[A] = ps.map(pa => pa(e).get)
      UnitFuture(la)
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
