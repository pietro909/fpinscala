package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapWithFM[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def doubleWithMap: Rand[Double] =
    map[Int, Double](nonNegativeInt)(i => (i.toDouble - 1.0) / Int.MaxValue)

  // discussing it on gitter 
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (n, rng2) if (n >= 0 && n <= Int.MaxValue) => (n, rng2)
      case (_, rng2) => nonNegativeInt(rng2)
    }

  // my original implementation: I should think a bit more...
  def oDouble(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = rng.nextInt
    val r =
      if (n > 0) ((n - 1).toDouble / Int.MaxValue)
      else -n.toDouble / Int.MaxValue
    (r, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    ((n.toDouble - 1.0) / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(r1: RNG, acc: List[Int]): (List[Int], RNG) =
      if (acc.length == count) (acc, r1)
      else {
        val (n, r2) = r1.nextInt
        loop(r2, n::acc)
      }
    loop(rng, List())
  }

  def intsWithSeq(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)((r: RNG) => r.nextInt))
  // sequence(List.fill(count)(int))
  
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // in this solution, RNG is still explicit, even if I'm not using it but passing through
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      fs match {
        case h::t =>
          val (a, _) = h(rng)
          val (rla, _) = sequence(t)(rng)
          (a::rla, rng)
        case Nil =>
          (Nil, rng)
      }

  // Rand : RNG => (List[A], RNG)
  // RNG is apparently gone: it is now implicit in the Rand[A] type.
  def sequenceWithFR[A](fs: List[Rand[A]]): Rand[List[A]] = {
    type R = Rand[List[A]]
    val start: R = unit(List[A]())
    def consIt: (A, List[A]) => List[A] = _ :: _
    def f: (Rand[A], R) => R =
      (ra, rla) => map2[A, List[A], List[A]](ra, rla)(consIt)
    fs.foldRight[R](start)(f)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i => 
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
