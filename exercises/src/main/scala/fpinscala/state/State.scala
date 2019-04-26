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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //  def nonNegativeInt: Rand[Int] =
  //    rng => rng.nextInt match {
  //      case (Int.MinValue, s) => (Int.MaxValue, s)
  //      case (v, s) => (Math.abs(v), s)
  //    }

  def nonNegativeInt: Rand[Int] =
    map(int)(math.abs)

  //  def double(rng: RNG): (Double, RNG) = {
  //    val r = RNG.nonNegativeInt(rng)
  //    (r._1.toDouble / Int.MaxValue.toDouble, r._2)
  //  }

  def double: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble)

  //  def intDouble(rng: RNG): ((Int, Double), RNG) = {
  //    val n = rng.nextInt
  //    val m = RNG.double(n._2)
  //
  //    ((n._1, m._1), m._2)
  //  }

  def intDouble: Rand[(Int, Double)] = both(int, double)

  //  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  //    val n = RNG.double(rng)
  //    val m = n._2.nextInt
  //
  //    ((n._1, m._1), m._2)
  //  }

  def doubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val n = RNG.double(rng)
    val m = RNG.double(n._2)
    val l = RNG.double(m._2)

    ((n._1, m._1, l._1), l._2)
  }

//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    if (count <= 0)
//      (Nil, rng)
//    else {
//      val (x, nextRng) = rng.nextInt
//      val (subList, finalRng) = ints(count - 1)(nextRng);
//
//      (x :: subList, finalRng)
//    }
//  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)

      (f(a, b), rngb)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((a, b) => (a, b))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(Nil): Rand[List[A]])(
      (acc, elem) => {
        map2(acc, elem)(
          (valueList, value) => valueList ++ List(value)
        )
      }
    )

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
