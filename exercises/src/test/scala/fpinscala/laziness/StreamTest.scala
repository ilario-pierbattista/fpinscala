package fpinscala.laziness

import utest._


object StreamTest extends TestSuite {
  def isEven: Int => Boolean = _ % 2 == 0

  val tests = Tests {
    'toList - {
      assert(List() == Stream().toList)
      assert(List(1, 2, 3) == Stream(1, 2, 3).toList)
    }

    'take - {
      assert(List() == Stream().take(2).toList)
      assert(List(1, 2) == Stream(1, 2, 3, 4).take(2).toList)
    }

    'drop - {
      assert(List() == Stream().drop(2).toList)
      assert(List(3, 4) == Stream(1, 2, 3, 4).drop(2).toList)
    }

    'takeWhile - {
      assert(List() == (Stream(): Stream[Int]).takeWhile(_ <= 2).toList)
      assert(List(1, 2) == Stream(1, 2, 3, 4).takeWhile(_ <= 2).toList)
      assert(List(1, 2) == Stream(1, 2, 3, 4, -1, -3).takeWhile(_ <= 2).toList)
    }

    'forAll - {
      assert(Stream(2, 4, 6).forAll(isEven))
      assert(!Stream(2, 3, 6).forAll(isEven))
    }

    'headOption - {
      assert(Stream().headOption.isEmpty)
      assert(Stream(1, 2, 3).headOption.contains(1))
    }

    'map - {
      assert(List() == (Stream(): Stream[Int]).map(_ * 2).toList)
      assert(List(2, 4, 6) == Stream(1, 2, 3).map(_ * 2).toList)
    }

    'filter - {
      assert(List(2, 4) == Stream(1, 2, 3, 4).filter(isEven).toList)
    }

    'flatMap - {
      assert(List() == (Stream(): Stream[Int]).flatMap(x => Stream(x, x * 2)).toList)
      assert(List(1, 2, 2, 4, 3, 6) == Stream(1, 2, 3).flatMap(x => Stream(x, x * 2)).toList)
      assert(List(1, 3) == Stream(1, 2, 3).flatMap(x => if (x == 2) Empty else Stream(x)).toList)
    }

    'append - {
      assert(List(1, 2, 3, 4) == Stream(1, 2).append(Stream(3, 4)).toList)
    }

    'ones - {
      assert(List(1, 1, 1) == Stream.ones.take(3).toList)
    }

    'constant - {
      assert(List(2, 2, 2) == Stream.constant(2).take(3).toList)
    }

    'from - {
      assert(List(2, 3, 4) == Stream.from(2).take(3).toList)
    }

    'fibs - {
      assert(
        List(1, 1, 2, 3, 5, 8) == Stream.fib().take(6).toList
      )
    }

    'zipWith - {
      assert(
        List("1a", "2b", "3c") == Stream(1, 2, 3).zipWith(Stream('a', 'b', 'c'))((i, c) => s"$i$c").toList
      )
    }

    'zipAll - {
      assert(
        List(
          (Some(1), Some('a')),
          (Some(2), Some('b')),
          (Some(3), Some('c'))
        ) == Stream(1, 2, 3).zipAll(Stream('a', 'b', 'c')).toList
      )
      assert(
        List(
          (Some(1), Some('a')),
          (Some(2), None),
          (Some(3), None)
        ) == Stream(1, 2, 3).zipAll(Stream('a')).toList
      )
      assert(
        List(
          (Some(1), Some('a')),
          (Some(2), Some('b')),
          (None, Some('c'))
        ) == Stream(1, 2).zipAll(Stream('a', 'b', 'c')).toList
      )
    }

    'hasSubsequence - {
      assert(Stream(1, 2, 3, 4).hasSubsequence(Empty))
      assert(Stream(1, 2, 3, 4).hasSubsequence(Stream(2, 3)))
      assert(!Stream(1, 2, 3, 4).hasSubsequence(Stream(3, 5)))
    }

    'startsWith - {
      assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
      assert(Stream(1, 2, 3).startsWith(Stream()))
      assert(!Stream(1, 2, 3).startsWith(Stream(2, 3)))
      assert(!Stream(1).startsWith(Stream(1, 2, 3)))
    }

    'tails - {
      assert(
        List(
          List(2, 3),
          List(3),
          List(): List[Int]
        ) == Stream(1, 2, 3)
          .tails
          .toList
          .map(_.toList)
      )
    }

    'scanRight - {
      assert(
        List(6, 5, 3, 0) == Stream(1, 2, 3).scanRight(0)(_ + _).toList
      )
    }
  }
}
