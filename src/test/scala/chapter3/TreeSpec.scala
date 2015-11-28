package chapter3

import org.scalatest.{Matchers, FreeSpec}

class TreeSpec extends FreeSpec with Matchers {

  "Function `size` should return the number of nodes (leaves and branches) in a tree" in {
    val one = Leaf(1)
    Tree.size(one) shouldBe 1

    val three = Branch(Leaf(1), Leaf(1))
    Tree.size(three) shouldBe 3

    val seven = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))
    Tree.size(seven) shouldBe 7

    val nine = Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Branch(Leaf(1), Leaf(1)))
    Tree.size(nine) shouldBe 9
  }

  "Function `maximum` should return the maximum element in a Tree[Int]" in {
    Tree.maximum( Branch(Leaf(1), Leaf(100)) ) shouldBe 100
    Tree.maximum( Branch(Branch(Leaf(1), Leaf(100)), Leaf(1000))) shouldBe 1000
    Tree.maximum(
      Branch(
        Branch(
          Branch(Leaf(100), Leaf(1)),
          Branch(Leaf(100), Branch(Leaf(1), Leaf(99)))
        ),
        Leaf(99)
      )
    ) shouldBe 100
  }

  "Function `depth` should return the maximum path length from the root of a tree to any leaf" in {
    val one = Leaf(0)
    Tree.depth(one) shouldBe 1

    val two = Branch(Leaf(1), Leaf(1))
    Tree.depth(two) shouldBe 2

    val three = Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))
    Tree.depth(three) shouldBe 3

    val four = Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(1))))
    Tree.depth(four) shouldBe 4
  }

  "Function `map` should transform each element in a tree with a given function" in {
    Tree.map(Leaf(1))(_ + 100) shouldBe Leaf(101)
    Tree.map(Branch(Leaf(1), Leaf(2)))(_ * 2) shouldBe Branch(Leaf(2), Leaf(4))
    Tree.map(Branch(Leaf(1), Branch(Leaf(5), Leaf(10))))(_ * 2) shouldBe Branch(Leaf(2), Branch(Leaf(10), Leaf(20)))
  }

  "Functions defined using fold should work the same as functions defined previously" - {
    "sizeUsingFold and size" in {
      val one = Leaf(1)
      val three = Branch(Leaf(1), Leaf(1))
      val seven = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))
      val nine = Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Branch(Leaf(1), Leaf(1)))

      Tree.size(one) shouldBe Tree.sizeUsingFold(one)
      Tree.size(three) shouldBe Tree.sizeUsingFold(three)
      Tree.size(seven) shouldBe Tree.sizeUsingFold(seven)
      Tree.size(nine) shouldBe Tree.sizeUsingFold(nine)
    }
    "depthUsingFold and depth" in {
      val one = Leaf(0)
      val two = Branch(Leaf(1), Leaf(1))
      val three = Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))
      val four = Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(1))))

      Tree.depth(one) shouldBe Tree.depthUsingFold(one)
      Tree.depth(two) shouldBe Tree.depthUsingFold(two)
      Tree.depth(three) shouldBe Tree.depthUsingFold(three)
      Tree.depth(four) shouldBe Tree.depthUsingFold(four)
    }
    "maximum using fold" in {
      val tree1 = Branch(Leaf(1), Leaf(100))
      val tree2 = Branch(Branch(Leaf(1), Leaf(100)), Leaf(1000))
      val tree3 = Branch(Branch(Branch(Leaf(100), Leaf(1)), Branch(Leaf(100), Branch(Leaf(1), Leaf(99)))), Leaf(99))

      Tree.maximum(tree1) shouldBe Tree.maximumUsingFold(tree1)
      Tree.maximum(tree2) shouldBe Tree.maximumUsingFold(tree2)
      Tree.maximum(tree3) shouldBe Tree.maximumUsingFold(tree3)
    }
    "map using fold" in {
      Tree.map(Leaf(1))(_ + 100) shouldBe Tree.mapUsingFold(Leaf(1))(_ + 100)

      Tree.map(Branch(Leaf(1), Leaf(2)))(_ * 2) shouldBe Tree.mapUsingFold(Branch(Leaf(1), Leaf(2)))(_ * 2)

      Tree.map(Branch(Leaf(1), Branch(Leaf(5), Leaf(10))))(_ * 2) shouldBe
        Tree.mapUsingFold(Branch(Leaf(1), Branch(Leaf(5), Leaf(10))))(_ * 2)
    }

  }


}
