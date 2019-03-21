package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def fold[A, B](t: Tree[A])(leaf: A => B, branch: (Tree[A], Tree[A]) => B): B =
    t match {
      case Leaf(value) => leaf(value)
      case Branch(l, r) => branch(l, r)
    }

  /* def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }*/

  def size[A](t: Tree[A]): Int =
    fold(t)(
      _ => 1,
      (l, r) => 1 + size(l) + size(r)
    )

  /*def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }*/

  def maximum(t: Tree[Int]): Int =
    fold(t)(
      identity,
      (l, r) => maximum(l) max maximum(r)
    )

  /*def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }*/

  def depth[A](t: Tree[A]): Int =
    fold(t)(
      _ => 0,
      (l, r) => 1 + (depth(l) max depth(r))
    )

  /*def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }*/

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(
      v => Leaf(f(v)),
      (l, r) => Branch(map(l)(f), map(r)(f))
    )
}