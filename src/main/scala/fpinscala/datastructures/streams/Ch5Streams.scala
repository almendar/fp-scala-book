package fpinscala.datastructures.streams

import scala.collection.immutable.Stream.Empty


object LazyPairMaker {

	def make(i: => Int) =  {
		val j : Int = i;
		(j,j)
	} 
}



trait Stream[+A] {


  override def toString() : String = uncons match {
    case None => "Empty"
    case Some((t,s)) => s"Stream($t,$s)"
  }

	def uncons: Option[(A,Stream[A])]
	def isEmpty : Boolean = uncons.isEmpty
	def toList : List[A] = uncons match {
		case None => Nil
		case Some((a,stream)) => a :: stream.toList
	}
	def take(n:Int) : Stream[A] = uncons match {
		case None => Stream.empty
		case Some((a,stream)) => if(n > 0) Stream.cons(a,stream.take(n-1)) else Stream.empty
	}

	def takeWhile(p : A => Boolean) : Stream[A] = foldRight(Stream.empty : Stream[A])((a,b) => if(!p(a)) b else Stream.cons(a,b))

	// uncons match {
	// 	case None => Stream.empty
	// 	case Some((a,stream)) => if(p(a)) Stream.cons(a,stream.takeWhile(p)) else Stream.empty
	// }

  def zipWith[B,C](os : Stream[B])(f : (A,B) => C) : Stream[C] = Stream.unfold((this,os))({s =>
    (s._1.uncons,s._2.uncons) match {
      case (Some((el_this,s_this)),Some((el_os,s_os))) => Some((f(el_this,el_os), (s_this,s_os)))
      case _ => None
    }
  })

//    (this.uncons,os.uncons) match {
//    case (Some((el_this,s_this)),Some((el_os,s_os))) => Stream.cons(f(el_this,el_os), s_this.zipWith(s_os)(f))
//    case ((_,_)) => Stream.empty
//  }

  def zip[B](os:Stream[B]) : Stream[(A,B)] = zipWith(os)((a,b) => (a,b))

//  def zipWithAll[B >: A](os : Stream[B]) : Stream[(A,B)] = (this.uncons,os.uncons) match {
//    case (Some((el_this,s_this)),Some((el_os,s_os))) => Stream.cons((el_this,el_os), s_this.zip(s_os))
//    case ((None,None)) => Stream.empty
//  }

  def tails : Stream[Stream[A]] = Stream.unfold((this,true))(p=> {
    val (s,b) = p
    s.uncons match {
      case Some((el,tail)) => Some(s,(tail,true))
      case None if(b) => Some(Stream.empty,(Stream.empty,false))
      case None if(!b) => None
    }
  })

  def scanRight[B](acc : B)(f : (A,B) => B) : Stream[B] = tails.map(_.foldRight(acc)((a,b) => f(a,b)))


	def foldRight[B](z: => B)(f: (A, => B) => B) : B = uncons match {
		case Some((h,t)) => f(h,t.foldRight(z)(f))
		case None => z
	}
	def append[B >: A](other : => Stream[B]) : Stream[B] = foldRight(other)((a,b) => Stream.cons(a,b))
	def exists(p : A => Boolean) : Boolean = foldRight(false)((a,b) => p(a) || b)
	def forAll(p : A => Boolean) : Boolean = foldRight(true)((a,b) =>  p(a) && b)
	def map[B](p : A => B) : Stream[B] = foldRight(Stream.empty : Stream[B])((a,b) => Stream.cons(p(a),b))
	def filter(p : A => Boolean) : Stream[A] = foldRight(Stream.empty : Stream[A])((a,b) => if(p(a)) Stream.cons(a,b) else b)
	def flatMap[B](p : A => Stream[B]) : Stream[B] = foldRight(Stream.empty : Stream[B])((a,b) => p(a) append b )
	def startsWith[B >: A](os:Stream[B]) : Boolean = zipWith(os)(_==_).forAll(_ == true)
}

object Stream {
	def empty[A] : Stream[A] = new Stream[A]{ def uncons = None }
	def cons[A](hd: => A, tl: => Stream[A]) : Stream[A] = {new Stream[A]{ lazy val uncons = Some(hd,tl)} }
	def apply[A](xs : A*) : Stream[A] = if(xs.isEmpty) empty[A] else cons(xs.head,apply(xs.tail : _*))
	def constant[A](a : A) : Stream[A] = cons(a,constant(a))
	def from(n: Int): Stream[Int] = cons(n,from(n+1))
	def fibs : Stream[Int] = {
		def go(n1:Int,n2:Int) : Stream[Int] = {
			cons(n1,go(n2,n1+n2))
		}
		go(0,1)
	}

	def unfold[A,S](z:S)(f : S => Option[(A,S)]) : Stream[A] = f(z) match {
		case Some((a,s)) => cons(a,Stream.unfold(s)(f))
		case None => Stream.empty 
	}
	def fibs1 = unfold((0,1))(s => Some((s._1,(s._2,s._1+s._2))))
}
