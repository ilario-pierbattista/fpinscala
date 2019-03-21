package fpinscala.datastructures

import utest._

object TreeTest extends TestSuite {
  val tests = Tests {
    val oneLeaf = Leaf(1)

    val simpleBranch = Branch(
      Leaf(1),
      Leaf(2)
    )

    val simpleTree = Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Leaf(3)
    )

    'size - {
      assert(1 == Tree.size(oneLeaf))
      assert(3 == Tree.size(simpleBranch))
      assert(5 == Tree.size(simpleTree))
    }

    'maximum - {
      assert(1 == Tree.maximum(oneLeaf))
      assert(2 == Tree.maximum(simpleBranch))
      assert(3 == Tree.maximum(simpleTree))
    }

    'dept - {
      assert(0 == Tree.depth(oneLeaf))
      assert(1 == Tree.depth(simpleBranch))
      assert(2 == Tree.depth(simpleTree))
    }

    'map - {
      assert(Leaf(2) == Tree.map(oneLeaf)(_ * 2))
      assert(Branch(Leaf(2), Leaf(4)) == Tree.map(simpleBranch)(_ * 2))
      assert(Branch(Branch(Leaf(2), Leaf(4)), Leaf(6)) == Tree.map(simpleTree)(_ * 2))
    }
  }
}
