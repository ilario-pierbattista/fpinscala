package fpinscala.state

import utest._

object StateTest extends TestSuite {
  val tests = Tests {
    'base - {
      val (a, rnga) = RNG.Simple(42).nextInt
      val (b, rngb) = rnga.nextInt
      val (n, _) = rngb.nextInt
      assert(16159453 == a)
      assert(-1281479697 == b)
      assert(-340305902 == n)
    }

    'ints - {
      assertMatch(
        RNG.ints(3)(RNG.Simple(42))
      ) {
        case (List(16159453, -1281479697, -340305902), _) =>
      }
    }

//    'simulateMachine - {
//      State.unit(Machine(true, 5, 10))
//    }
  }
}
