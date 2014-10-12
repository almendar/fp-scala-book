package fpinscala.tests.datastructures

import org.scalatest.FlatSpec
import fpinscala.datastructures.tree.{Tree,Branch,Leaf}

class Ch3TreesTest extends FlatSpec {

    val sampleTree = Branch(
            Branch(
                    Leaf(1),
                    Branch(
                            Leaf(21),
                            Branch(
                                    Leaf(31),
                                    Leaf(32)
                            )
                    )
            ),
            Leaf(4))


  "A tree" should ".size" in {
    assert(sampleTree.size === 5)
  }
  it should ".maximum" in {
    assert(sampleTree.maximum === 32)
  }
  it should ".depth" in {
    assert(sampleTree.depth === 5)
  }
  it should ".map" in {
    assert(sampleTree.map(x=>x+1) === Branch(
      Branch(
        Leaf(2),
        Branch(
          Leaf(22),
          Branch(
            Leaf(32),
            Leaf(33)
          )
        )
      ),
      Leaf(5)))
  }
}
