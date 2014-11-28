package fpinscala


object LazyPairMaker {

	def make(i: => Int) =  {
		val j : Int = i;
		(j,j)
	} 
}



trait StreamA[+A] {


  override def toString() : String = uncons match {
    case None => "Empty"
    case Some((t,s)) => s"StreamA($t,$s)"
  }

	def uncons: Option[(A,StreamA[A])]
	def isEmpty : Boolean = uncons.isEmpty
	def toList : List[A] = uncons match {
		case None => Nil
		case Some((a,stream)) => a :: stream.toList
	}
	def take(n:Int) : StreamA[A] = uncons match {
		case None => StreamA.empty
		case Some((a,stream)) => if(n > 0) StreamA.cons(a,stream.take(n-1)) else StreamA.empty
	}

	def takeWhile(p : A => Boolean) : StreamA[A] = foldRight(StreamA.empty : StreamA[A])((a,b) => if(!p(a)) b else StreamA.cons(a,b))

	// uncons match {
	// 	case None => StreamA.empty
	// 	case Some((a,stream)) => if(p(a)) StreamA.cons(a,stream.takeWhile(p)) else StreamA.empty
	// }

  def zipWith[B,C](os : StreamA[B])(f : (A,B) => C) : StreamA[C] = StreamA.unfold((this,os))({s =>
    (s._1.uncons,s._2.uncons) match {
      case (Some((el_this,s_this)),Some((el_os,s_os))) => Some((f(el_this,el_os), (s_this,s_os)))
      case _ => None
    }
  })

//    (this.uncons,os.uncons) match {
//    case (Some((el_this,s_this)),Some((el_os,s_os))) => StreamA.cons(f(el_this,el_os), s_this.zipWith(s_os)(f))
//    case ((_,_)) => StreamA.empty
//  }

  def zip[B](os:StreamA[B]) : StreamA[(A,B)] = zipWith(os)((a,b) => (a,b))

//  def zipWithAll[B >: A](os : StreamA[B]) : StreamA[(A,B)] = (this.uncons,os.uncons) match {
//    case (Some((el_this,s_this)),Some((el_os,s_os))) => StreamA.cons((el_this,el_os), s_this.zip(s_os))
//    case ((None,None)) => StreamA.empty
//  }

  def tails : StreamA[StreamA[A]] = StreamA.unfold((this,true))(p=> {
    val (s,b) = p
    s.uncons match {
      case Some((el,tail)) => Some(s,(tail,true))
      case None if(b) => Some(StreamA.empty,(StreamA.empty,false))
      case None if(!b) => None
    }
  })

  def scanRight[B](acc : B)(f : (A,B) => B) : StreamA[B] = tails.map(_.foldRight(acc)((a,b) => f(a,b)))


	def foldRight[B](z: => B)(f: (A, => B) => B) : B = uncons match {
		case Some((h,t)) => f(h,t.foldRight(z)(f))
		case None => z
	}
	def append[B >: A](other : => StreamA[B]) : StreamA[B] = foldRight(other)((a,b) => StreamA.cons(a,b))
	def exists(p : A => Boolean) : Boolean = foldRight(false)((a,b) => p(a) || b)
	def forAll(p : A => Boolean) : Boolean = foldRight(true)((a,b) =>  p(a) && b)
	def map[B](p : A => B) : StreamA[B] = foldRight(StreamA.empty : StreamA[B])((a,b) => StreamA.cons(p(a),b))
	def filter(p : A => Boolean) : StreamA[A] = foldRight(StreamA.empty : StreamA[A])((a,b) => if(p(a)) StreamA.cons(a,b) else b)
	def flatMap[B](p : A => StreamA[B]) : StreamA[B] = foldRight(StreamA.empty : StreamA[B])((a,b) => p(a) append b )
	def startsWith[B >: A](os:StreamA[B]) : Boolean = zipWith(os)(_==_).forAll(_ == true)
}

object StreamA {
	def empty[A] : StreamA[A] = new StreamA[A]{ def uncons = None }
	def cons[A](hd: => A, tl: => StreamA[A]) : StreamA[A] = {new StreamA[A]{ lazy val uncons = Some(hd,tl)} }
	def apply[A](xs : A*) : StreamA[A] = if(xs.isEmpty) empty[A] else cons(xs.head,apply(xs.tail : _*))
	def constant[A](a : A) : StreamA[A] = cons(a,constant(a))
	def from(n: Int): StreamA[Int] = cons(n,from(n+1))
	def fibs : StreamA[Int] = {
		def go(n1:Int,n2:Int) : StreamA[Int] = {
			cons(n1,go(n2,n1+n2))
		}
		go(0,1)
	}

	def unfold[A,S](z:S)(f : S => Option[(A,S)]) : StreamA[A] = f(z) match {
		case Some((a,s)) => cons(a,StreamA.unfold(s)(f))
		case None => StreamA.empty 
	}
	def fibs1 = unfold((0,1))(s => Some((s._1,(s._2,s._1+s._2))))
}
