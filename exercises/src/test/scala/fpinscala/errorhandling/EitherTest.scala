package fpinscala.errorhandling

import utest._

import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


object EitherTest extends TestSuite {
  val tests = Tests {
    def divider(by: Double)(x: Double): Either[String, Double] =
      if (by != 0) Right(x / by)
      else Left("can't divide by zero")

    def duplicate(x: Double): Double = x * 2

    def isEven(x: Int): Boolean = x % 2 == 0

    'map - {
      assert(Left("error") == Left("error").map(duplicate))
      assert(Right(4.0) == Right(2.0).map(duplicate))
    }

    'flatMap - {
      assert(Left("error") == Left("error").flatMap(divider(0)))
      assert(Left("can't divide by zero") == Right(2.0).flatMap(divider(0)))
      assert(Left("error") == Left("error").flatMap(divider(2)))
      assert(Right(1.0) == Right(2.0).flatMap(divider(2.0)))
    }

    'orElse - {
      assert(Right(1) == Right(1).orElse(Right(2)))
      assert(Right(2) == Left("e").orElse(Right(2)))
      assert(Left("e2") == Left("e").orElse(Left("e2")))
    }

    'map2 - {
      def areEqual(a: Int, str: String): Boolean = a.toString == str

      assert(Left("e") == Left("e").map2(Left("e2"))(areEqual))
      assert(Left("e2") == Right(1).map2(Left("e2"))(areEqual))
      assert(Left("e") == Left("e").map2(Right("1"))(areEqual))
      assert(Right(true) == Right(1).map2(Right("1"))(areEqual))
    }
  }
}
