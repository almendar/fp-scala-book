package fpinscala.tests

import fpinscala.StreamA
import org.scalatest.FlatSpec

class Ch5StreamAsTests extends FlatSpec {
  val someStreamA = StreamA(1,2,3,4,5,6,7,8,9)


  "StreamA(1,2,3,4,5,6,7,8,9)" should ".takeWhile" in {
    assert(someStreamA.takeWhile(_ < 5).toList === List(1,2,3,4))
  }
  it should ".forAll" in {
    assert(someStreamA.forAll(_ < 2) === false)
  }

  it should ".filter" in {
    assert(someStreamA.filter(_ % 2 == 0).toList === List(2,4,6,8))
  }

  it should ".take" in {
    assert(someStreamA.take(3).toList === List(1,2,3))
  }

  it should ".constant" in {
    assert(StreamA.constant(2).take(5).toList === List(2,2,2,2,2))
  }

  it should ".from" in {
    assert(StreamA.from(6).take(4).toList === List(6,7,8,9))
  }

  it should ".fibs" in {
    assert(StreamA.fibs.take(15).toList === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377))
    assert(StreamA.fibs1.take(15).toList === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377))
  }

  it should ".zipWith" in {
    assert(
       (StreamA(1,2,3,4,6,7,8,9).zipWith(StreamA("1","2","3"))(_ * _.toInt)).toList === List(1,4,9)
    )
  }
  it should ".zip" in {
    assert(
       (StreamA(1,2,3,4,6,7,8,9) zip StreamA("1","2","3")).toList === List((1,"1"),(2,"2"),(3,"3"))
    )
  }

  it should ".startsWith" in {
      assert(StreamA(1,2,3) startsWith StreamA(1,2,3,4,5,6))
  }
  it should ".tails" in {
    assert(StreamA(1,2,3).tails.map(_.toList).toList ===

    List(List(1,2,3),
      List(2,3),
      List(3),Nil
      ).toList)
  }

  it should ".scanRight" in {
    assert(StreamA(1,2,3).scanRight(0)(_ + _).toList === List(6,5,3,0))
  }

}
