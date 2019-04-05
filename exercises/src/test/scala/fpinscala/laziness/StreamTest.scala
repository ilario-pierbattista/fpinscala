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
      assert(List(1, 2, 3, 4) == Stream(1, 2).append(() => Stream(3, 4)).toList)
    }
  }
}
