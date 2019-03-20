package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
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

  val result = List(1, 2, 3, 4, 5) match {
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

  /*def append[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)(
      (x, acc) => acc match {
        case Nil => Cons(x, Nil)
        case Cons(h,t) => Cons(x, Cons(h, t))
      }
    )*/

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
  // foldRight(ns, 0)((x, y) => x + y)
    foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
  // foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
    foldRight(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, tail) => tail
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, tail) => Cons(h, tail)
    case Nil => List(h)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(_, tail) if n > 0 => drop(tail, n - 1)
      case Cons(h, t) if n == 0 => Cons(h, t)
      case Nil => Nil
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, tail) if f(h) => dropWhile(tail, f)
      case Cons(h, tail) if !f(h) => Cons(h, tail)
      case Nil => Nil
    }

  dropWhile(List(1, 2, 3), (x: Int) => x % 2 == 0)

  def dropWhileInferred[A](l: List[A])(f: A => Boolean): List[A] = {
    dropWhile(l, f)
  }

  dropWhileInferred(List(1, 2, 3))(x => x % 2 == 0)

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case Nil => Nil
    }

  def length[A](l: List[A]): Int = {
    // Old implementation based on foldRight
    // foldRight(l, 0)((_, c) => c + 1)

    foldLeft(l, 0)((c, _) => c + 1)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  /** Implementazione in termini di foldRight */
  // def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  //  foldRight(List.reverse(l), z)((x, c) => f(c, x))

  def reverse[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A])(
      (x, acc) => append(acc, List(x))
    )

  def flattern2[A](x: List[List[A]]): List[A] =
    foldLeft(x, Nil: List[A])(
      (acc, xs) => List.append(acc, xs)
    )

  def listIncrementerBy1(xs: List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int])(
      (x, res) => Cons(x + 1, res)
    )

  def stringifyDouble(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String])(
      (x, acc) => Cons(x.toString, acc)
    )

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])(
      (x, acc) => Cons(f(x), acc)
    )

  /*def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])(
      (x, acc) =>
        if (f(x)) Cons(x, acc)
        else acc
    )*/

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    List.flatMap(l)(x => if (f(x)) List(x) else Nil)

  def flatMap[A](xs: List[A])(f: A => List[A]): List[A] =
    foldRight(xs, Nil: List[A])(
      (x, acc) => append(f(x), acc)
    )

  def zipIntegers(lx: List[Int], ly: List[Int]): List[Int] =
    (lx, ly) match {
      case (Nil, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipIntegers(xs, ys))
      // Questa è una schifezza, ma per il momento mi serve solo a non fare incazzare il compilatore
      case (Cons(_, _), Nil) => throw new Error
      case (Nil, Cons(_, _)) => throw new Error
    }

  def zip[A, B](lx: List[A], ly: List[B]): List[(A, B)] =
    (lx, ly) match {
      case (Nil, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons((x, y), zip(xs, ys))
      // Questa è una schifezza, ma per il momento mi serve solo a non fare incazzare il compilatore
      case (Cons(_, _), Nil) => throw new Error
      case (Nil, Cons(_, _)) => throw new Error
    }

  def zipWith[A, B, C](lx: List[A], ly: List[B])(f: (A, B) => C): List[C] =
    map(zip(lx, ly)) {
      case (x, y) => f(x, y)
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def truncate(xs: List[A], len: Int): List[A] =
      xs match {
        case Cons(_, _) if len == 0 => Nil
        case Cons(h, t) if len > 0 => Cons(h, truncate(t, len - 1))
        case Nil => Nil
      }

    def duplicator(xs: List[A], len: Int): List[List[A]] = {
      def doDuplication(xs: List[A], len: Int, acc: List[List[A]]): List[List[A]] =
        xs match {
          case Cons(h, t) if length(Cons(h, t)) >= len => Cons(truncate(Cons(h, t), len), doDuplication(t, len, acc))
          case Cons(_, _) => acc
          case Nil => acc
        }

      doDuplication(xs, len, Nil: List[List[A]])
    }

    val candidates = duplicator(sup, length(sub))

    foldLeft(candidates, false)(
      (acc, candidate) => {
        acc || candidate == sub
      }
    )
  }
}
