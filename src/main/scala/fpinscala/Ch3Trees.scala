package fpinscala

sealed trait Tree[+A] {
  def fold[B](f: (A) => B)(g : (B,B) => B) : B = {
    this match {
      case Leaf(x) =>	f(x)
      case Branch(b1:Tree[A],b2:Tree[A]) => g(b1.fold(f)(g),b2.fold(f)(g))
    }
  }

  def size : Int = fold(x => 1)(_+_)
  // {
  // 	t match {
  // 		case Leaf(x) => 1
  // 		case Branch(b1,b2) => size(b1) + size(b2)
  // 	}
  // }

  def maximum[B >: A](implicit num: Numeric[B]) : B = fold(x=>x:B)((x1,x2) => num.max(x1,x2))
  // {
  // 	t match {
  // 		case Leaf(x) => x
  // 		case Branch(b1,b2) => maximum(b1) max maximum(b2)
  // 	}
  // }

  def depth : Int = fold(x=> 1)((b1,b2) => (b1+1) max (b2+1))
  // {
  // 	t match {
  // 		case Leaf(x) => 1
  // 		case Branch(b1,b2) => (depth(b1) + 1)  max  (depth(b2) + 1)
  // 	}
  // }

  def map[B](f: A => B) : Tree[B] = fold(x => Leaf(f(x)):Tree[B])((b1,b2) => Branch(b1,b2))
  //{
  //t match {
  // 	case Leaf(x) => Leaf(f(x))
  // 	case Branch(b1,b2) => Branch(map(b1)(f), map(b2)(f))
  // }


}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
