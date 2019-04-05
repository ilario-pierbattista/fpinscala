package fpinscala.laziness

import java.util.function.Predicate

import utest._

import scala.{Option => _, Some => _, Stream => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


object StreamTest extends TestSuite {
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
      val isEven: Int => Boolean = _ % 2 == 0

      assert(Stream(2, 4, 6).forAll(isEven))
      assert(!Stream(2, 3, 6).forAll(isEven))
    }
  }
}
