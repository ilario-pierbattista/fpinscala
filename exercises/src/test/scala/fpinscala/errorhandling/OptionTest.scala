package fpinscala.errorhandling

import utest._

import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


object OptionTest extends TestSuite {
  val tests = Tests {
    def divider(by: Double)(x: Double): Option[Double] =
      if (by != 0) Some(x / by)
      else None

    def duplicate(x: Double): Double = x * 2

    def isEven(x: Int): Boolean = x % 2 == 0

    'map - {
      assert(None == None.map(duplicate))
      assert(Some(4.0) == Some(2.0).map(duplicate))
    }

    'getOrElse - {
      assert(2.0 == Some(2.0).getOrElse(0))
      assert(0 == None.getOrElse(0))
    }

    'flatMap - {
      assert(None == None.flatMap(divider(0)))
      assert(None == Some(2.0).flatMap(divider(0)))
      assert(None == None.flatMap(divider(2)))
      assert(Some(1.0) == Some(2.0).flatMap(divider(2.0)))
    }

    'orElse - {
      assert(Some(1) == Some(1).orElse(Some(2)))
      assert(Some(2) == None.orElse(Some(2)))
      assert(None == None.orElse(None))
    }

    'filter - {
      assert(None == Some(1).filter(isEven))
      assert(Some(2) == Some(2).filter(isEven))
      assert(None == None.filter(isEven))
    }

    'variance - {
      assert(None == Option.variance(Seq()))
    }
  }
}
