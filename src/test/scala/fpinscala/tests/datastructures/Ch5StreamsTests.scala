package fpinscala.tests.datastructures
import fpinscala.datastructures.streams.Stream._
import fpinscala.datastructures.streams.Stream

import org.scalatest.FlatSpec

class Ch5StreamsTests extends FlatSpec {
  val someStream = Stream(1,2,3,4,5,6,7,8,9)


  "Stream(1,2,3,4,5,6,7,8,9)" should ".takeWhile" in {
    assert(someStream.takeWhile(_ < 5).toList === List(1,2,3,4))
  }
  it should ".forAll" in {
    assert(someStream.forAll(_ < 2) === false)
  }

  it should ".filter" in {
    assert(someStream.filter(_ % 2 == 0).toList === List(2,4,6,8))
  }

  it should ".take" in {
    assert(someStream.take(3).toList === List(1,2,3))
  }

  it should ".constant" in {
    assert(Stream.constant(2).take(5).toList === List(2,2,2,2,2))
  }

  it should ".from" in {
    assert(Stream.from(6).take(4).toList === List(6,7,8,9))
  }

  it should ".fibs" in {
    assert(Stream.fibs.take(15).toList === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377))
    assert(Stream.fibs1.take(15).toList === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377))
  }

  it should ".zipWith" in {
    assert(
       (Stream(1,2,3,4,6,7,8,9).zipWith(Stream("1","2","3"))(_ * _.toInt)).toList === List(1,4,9)
    )
  }
  it should ".zip" in {
    assert(
       (Stream(1,2,3,4,6,7,8,9) zip Stream("1","2","3")).toList === List((1,"1"),(2,"2"),(3,"3"))
    )
  }

  it should ".startsWith" in {
      assert(Stream(1,2,3) startsWith Stream(1,2,3,4,5,6))
  }
  it should ".tails" in {
    assert(Stream(1,2,3).tails.map(_.toList).toList ===

    List(List(1,2,3),
      List(2,3),
      List(3),Nil
      ).toList)
  }

  it should ".scanRight" in {
    assert(Stream(1,2,3).scanRight(0)(_ + _).toList === List(6,5,3,0))
  }

}
