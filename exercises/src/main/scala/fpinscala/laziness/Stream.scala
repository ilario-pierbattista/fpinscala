package fpinscala.laziness

import scala.annotation.tailrec

trait Stream[+A] {

  def toList: List[A] =
    this.fold(
      Nil: List[A],
      s => s.h() :: s.t().toList
    )

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def fold[B](whenEmpty: => B, whenCons: Cons[A] => B): B =
    this match {
      case Cons(h, t) => whenCons(Cons(h, t))
      case Empty => whenEmpty
    }

  //  def take(n: Int): Stream[A] =
  //    this.fold(
  //      Empty,
  //      { case Cons(h, t) =>
  //        if (n > 0) Cons(h, () => t().take(n - 1))
  //        else Empty
  //      }
  //    )

  def take(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, t), i) =>
        if (i > 0) Some((h(), (t(), i - 1)))
        else None
      case (_, _) => None
    }

  def drop(n: Int): Stream[A] =
    this.fold(
      Empty,
      {
        case Cons(h, t) =>
          if (n > 0) t().drop(n - 1)
          else Cons(h, t)
      }
    )

  /*
  def takeWhile(p: A => Boolean): Stream[A] =
    this.fold(
      Empty,
      { case Cons(h, t) =>
        if (p(h())) Cons(h, () => t().takeWhile(p))
        else Empty
      }
    )*/

  //  def takeWhile(p: A => Boolean): Stream[A] =
  //    this.foldRight(Empty: Stream[A])(
  //      (elem, acc) =>
  //        if (p(elem)) Cons(() => elem, () => acc)
  //        else Empty
  //    )

  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) =>
        if (p(h())) Some((h(), t()))
        else None
      case Empty => None
    }

  def forAll(p: A => Boolean): Boolean =
    this fold(
      true, {
      case Cons(h, t) if p(h()) => t().forAll(p)
      case _ => false
    })

  // Vincolo: usare foldRight
  def headOption: Option[A] =
    this.foldRight(None: Option[A])(
      (elem, _) => Some(elem)
    )

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  //  def map[B](f: A => B): Stream[B] =
  //    this.foldRight(Empty: Stream[B])(
  //      (elem, acc) => Cons(
  //        () => f(elem),
  //        () => acc
  //      )
  //    )

  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])(
      (elem, acc) =>
        if (p(elem)) Cons(() => elem, () => acc)
        else acc
    )

  def append[B >: A](other: => Stream[B]): Stream[B] =
    this.foldRight(other)(
      (elem, acc) => Cons(() => elem, () => acc)
    )

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Empty: Stream[B])(
      (elem, acc) => f(elem).append(acc)
    )

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(
        (
          f(h1(), h2()),
          (t1(), t2())
        )
      )
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case _ => None
    }

  def hasSubsequence[B >: A](sub: Stream[B]): Boolean =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(
        (Cons(h, t).zipAll(sub), t())
      )
    }.foldRight(false)(
      (zipped, found) =>
        found || zipped.forAll {
          case (Some(x), Some(y)) => x == y
          case (Some(_), None) => true
          case _ => false
        }
    )

  def startsWith[B](s: Stream[B]): Boolean = ???
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

  /*val ones: Stream[Int] = Stream.cons(1, ones) */

  val ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  /*def constant[A](v: A): Stream[A] = cons(v, constant(v))*/

  def constant[A](v: A): Stream[A] =
    unfold(null)(_ => Some((v, null)))

  /*def from(n: Int): Stream[Int] = cons(n, from(n + 1)) */

  def from(n: Int): Stream[Int] =
    unfold(n)(
      x => Some((x, x + 1))
    )

  /*
    def fib(): Stream[Int] = {
      def fibGenerator(currentIndex: Int, prevValues: (Int, Int)): Stream[Int] =
        currentIndex match {
          case 1 => cons(1, fibGenerator(2, (1, 0)))
          case 2 => cons(1, fibGenerator(3, (1, 1)))
          case i => cons(
            prevValues._1 + prevValues._2,
            fibGenerator(i + 1, (prevValues._1 + prevValues._2, prevValues._1)))
        }

      fibGenerator(1, (0, 0))
    }*/

  def fib(): Stream[Int] = {
    unfold((1, 0, 0)) {
      case (1, _, _) => Some((1, (2, 1, 0)))
      case (2, _, _) => Some((1, (3, 1, 1)))
      case (i, n, m) => Some((n + m, (i + 1, n + m, n)))
    }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def gen(state: S): Stream[A] = {
      f(state) match {
        case None => Empty
        case Some((value, newState)) => cons(value, gen(newState))
      }
    }

    gen(z)
  }
}