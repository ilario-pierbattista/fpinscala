package fpinscala.errorhandling

import java.lang.Exception

import utest._

import scala.util.control.Exception
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

    'map2 - {
      def areEqual(a: Int, str: String): Boolean = a.toString == str

      assert(None == Option.map2(None: Option[Int], None: Option[String])(areEqual))
      assert(None == Option.map2(Some(1): Option[Int], None: Option[String])(areEqual))
      assert(None == Option.map2(None: Option[Int], Some("1"): Option[String])(areEqual))
      assert(Some(true) == Option.map2(Some(1): Option[Int], Some("1"): Option[String])(areEqual))
    }

    'sequence - {
      assert(Some(List()) == Option.sequence(List()))
      assert(None == Option.sequence(List(Some(1), None, Some(3))))
      assert(Some(List(1, 2, 3)) == Option.sequence(List(Some(1), Some(2), Some(3))))
    }

    'traverse - {
      def parseInt(x: String): Option[Int] =
        try {
          Some(x.toInt)
        } catch {
          case e => None
        }

      assert(Some(List(1, 2, 3)) == Option.traverse(List("1", "2", "3"))(parseInt))
      assert(None == Option.traverse(List("1", "a", "3"))(parseInt))
      assert(Some(List()) == Option.traverse(List())(parseInt))
    }
  }
}
