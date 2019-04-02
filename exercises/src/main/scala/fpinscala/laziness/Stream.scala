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

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def fold[B, F[_]](
                     whenEmpty: => F[B],
                     whenCons: Cons[A] => F[B]
                   ): F[B] =
    this match {
      case Cons(h, t) => whenCons(Cons(h, t))
      case Empty => whenEmpty
    }

  def take(n: Int): Stream[A] =
    this.fold(
      Empty,
      cons =>
        if (n > 0) Cons(cons.h, () => cons.t().take(n - 1))
        else Empty
    )

  def drop(n: Int): Stream[A] =
    this.fold(
      Empty,
      {
        case Cons(h, t) =>
          if (n > 0) t().drop(n - 1)
          else Cons(h, t)
      }
    )

  def takeWhile(p: A => Boolean): Stream[A] =
    this.fold(
      Empty,
      { case Cons(h, t) =>
        if (p(h())) Cons(h, () => t().takeWhile(p))
        else Empty
      }
    )

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}