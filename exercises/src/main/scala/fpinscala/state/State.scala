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

  //  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  //    rng => {
  //      val (a, rng2) = s(rng)
  //      (f(a), rng2)
  //    }
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

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

  //  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  //    rng => {
  //      val (a, rnga) = ra(rng)
  //      val (b, rngb) = rb(rnga)
  //
  //      (f(a, b), rngb)
  //    }
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(
      a => flatMap(rb)(
        b => unit(
          f(a, b)
        )
      )
    )

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

  //  def nonNegativeLessThan(n: Int): Rand[Int] =
  //    rng => {
  //      val (a, rnga) = nonNegativeInt(rng)
  //      val mod = a % n
  //      if (a + (n - 1) - mod >= 0)
  //        (mod, rnga)
  //      else
  //        nonNegativeLessThan(n)(rnga)
  //    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(
      value => {
        val mod = value % n
        if (value + (n - 1) - mod >= 0)
          unit(mod)
        else
          nonNegativeLessThan(n)
      }
    )

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rnga) = f(rng)
      g(a)(rnga)
    }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(
      s => {
        val (a, sa) = run(s)
        (f(a), sa)
      }
    )

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap(
      a => sb.flatMap(
        b => State.unit(f(a, b))
      )
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, sa) = run(s)
        f(a).run(sa)
      }
    )
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def stateGet[S](): State[S, S] = State(s => (s, s))

  def stateSet[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- stateGet()
      _ <- stateSet(f(s))
    } yield ()

  def coin(): State[Machine, Unit] =
    modify {
      case Machine(true, candies, coins) if candies > 0 => Machine(locked = false, candies, coins + 1)
      case m => m
    }

  def turn(): State[Machine, Unit] =
    modify {
      case Machine(false, candies, coin) if candies > 0 => Machine(locked = true, candies - 1, coin)
      case m => m
    }

  def applyCommands(inputs: List[Input]): State[Machine, Unit] =
    inputs.foldLeft(State(m => ((), m)): State[Machine, Unit])(
      (acc, input) => acc.map2(
        input match {
          case Turn => turn()
          case Coin => coin()
        }
      )(
        (_, _) => ()
      )
    )

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    applyCommands(inputs)
      .map2(stateGet())(
        (_, machine) => (machine.candies, machine.coins)
      )
  }
}
