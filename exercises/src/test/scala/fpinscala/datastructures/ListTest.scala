package fpinscala.datastructures

import utest._

object ListTest extends TestSuite {
  val tests = Tests {
    'tail - {
      assert(List(2, 3) == List.tail(List(1, 2, 3)))
      assert(Nil == List.tail(Nil))
      assert(Nil == List.tail(List(1)))
    }

    'setHead - {
      assert(List(7, 2) == List.setHead(List(1, 2), 7))
      assert(List(7) == List.setHead(Nil, 7))
    }

    'drop - {
      assert(List(2, 3) == List.drop(List(1, 2, 3), 1))
      assert(List(3) == List.drop(List(1, 2, 3), 2))
      assert(Nil == List.drop(List(1, 2, 3), 3))
      assert(Nil == List.drop(List(1, 2, 3), 6))
      assert(Nil == List.drop(Nil, 23))
    }

    val isEven = (x: Int) => x % 2 == 0
    'dropWhile - {
      assert(List(1, 3, 5) == List.dropWhile(List(2, 4, 6, 1, 3, 5), isEven))
      assert(List(1, 3, 5, 8, 10) == List.dropWhile(List(2, 4, 6, 1, 3, 5, 8, 10), isEven))
      assert(List(1, 2, 3) == List.dropWhile(List(1, 2, 3), isEven))
      assert(Nil == List.dropWhile(List(2, 4, 6), isEven))
    }

    'init - {
      assert(List(1, 2) == List.init(List(1, 2, 3)))
      assert(Nil == List.init(List(1)))
      assert(Nil == List.init(Nil))
    }

    'lenght - {
      assert(0 == List.length(Nil))
      assert(0 == List.length(List()))
      assert(1 == List.length(List(1)))
      assert(2 == List.length(List(1, 2)))
    }

    'foldLeft - {
      assert(0 == List.foldLeft(Nil: List[Int], 0)(_ + _))
      assert(1 == List.foldLeft(List(1), 0)(_ + _))
      assert(10 == List.foldLeft(List(1, 2, 3, 4), 0)(_ + _))
    }

    'reverse - {
      assert(Nil == List.reverse(Nil))
      assert(List(1) == List.reverse(List(1)))
      assert(List(3, 2, 1) == List.reverse(List(1, 2, 3)))
    }

    'append - {
      assert(Nil == List.append(Nil, Nil))
      assert(List(1, 2) == List.append(List(1, 2), Nil))
      assert(List(3, 4) == List.append(Nil, List(3, 4)))
      assert(List(1, 2, 3, 4) == List.append(List(1, 2), List(3, 4)))
    }
  }
}
