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

  def take(n: Int): Stream[A] =
    this.fold(
      Empty,
      { case Cons(h, t) =>
        if (n > 0) Cons(h, () => t().take(n - 1))
        else Empty
      }
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

  /*
  def takeWhile(p: A => Boolean): Stream[A] =
    this.fold(
      Empty,
      { case Cons(h, t) =>
        if (p(h())) Cons(h, () => t().takeWhile(p))
        else Empty
      }
    )*/


  def takeWhile(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])(
      (elem, acc) =>
        if (p(elem)) Cons(() => elem, () => acc)
        else Empty
    )

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

  def map[B](f: A => B): Stream[B] =
    this.foldRight(Empty: Stream[B])(
      (elem, acc) => Cons(
        () => f(elem),
        () => acc
      )
    )

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