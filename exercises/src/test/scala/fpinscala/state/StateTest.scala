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

    'simulateMachine - {
      assertMatch(
        State.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn))
          .run(Machine(locked = true, coins = 10, candies = 5))
      ) {
        case ((2, 13), _) =>
      }

      assertMatch(
        State.simulateMachine(List(Turn, Turn, Turn, Coin, Coin, Coin, Turn))
          .run(Machine(locked = true, coins = 10, candies = 5))
      ) {
        case ((4, 11), _) =>
      }
    }
  }
}
