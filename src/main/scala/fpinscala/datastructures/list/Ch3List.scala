package fpinscala.datastructures.list


import List._

sealed trait List[+A] {
	def head : A
	def tail : List[A]
	def drop(n:Int) : List[A] = n match {
		case 0 => this
		case n1 => tail.drop(n-1)
	}
	def dropWhile(f: A => Boolean): List[A] = if( f(head) ) tail.dropWhile(f) else Cons(head,tail)
	def setHead[B >: A](b:B) : List[B] = Cons(b,tail)
	def init : List[A] =  {
		tail match {
			case Nil => Nil
			case _ => Cons(head,tail.init)
		}
	}

}

case object Nil extends List[Nothing] {
	def tail = Nil
	def head = throw new Exception("Empty list, cannot invoke .head on it")
	override def setHead[B](b:B) : List[B] = Nil
	override def drop(n:Int) = Nil
	override def init = Nil
	override def dropWhile(f: Nothing => Boolean) = Nil

}

case class Cons[+A](_head: A, _tail: List[A]) extends List[A]  {
	def head = _head
	def tail = this._tail
}

object List {
	def sum(ints: List[Int]): Int = ints match {
			case Nil => 0
			case Cons(x,xs) => x + sum(xs)
		}
	
	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x,xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
}
	
	def incremantByOne(lst : List[Int]) : List[Int] = {
		Nil
	}

	def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
		l match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}

	def length[A](l:List[A]) : Int = {
		foldRight(l,0)( (a:A,b:Int) => b+1 )
	}

	@annotation.tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
		l match {
			case Nil => z
			case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
		}
		
	}

	def reverse[A](l:List[A]) : List[A] = {
		foldLeft(l,Nil:List[A])((b,a) => Cons(a,b))
	}

	def map[A,B](l: List[A])(f: A => B): List[B] = {
		reverse(foldLeft(l,Nil:List[B])((acc,el) => Cons(f(el),acc)))
	}

	def flatten[A](ll:List[List[A]]) : List[A] = {
		reverse(foldLeft(ll,Nil:List[A])((acc,ell) => foldLeft(ell,acc)((acc1,el1)=>Cons(el1,acc1))))
	}

	def filter[A](l:List[A])(f : A => Boolean) : List[A] = {
		reverse( foldLeft(l,Nil:List[A])( (acc,ell) => if(f(ell)) Cons(ell,acc) else acc ) )
	}



	

}

//object Ch3List extends App {
//	val x = List(1,2,3,4,5) match {
//		case Cons(x, Cons(2, Cons(4, _))) => x
//		case Nil => 42
//		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//		case Cons(h, t) => h + sum(t)
//		case _ => 101
//	}
//
//	val sampleList = List(1,2,3,4,5)
//
//	println(x)
//	println(Nil)
//	println(List(1,2,3,4).tail)
//	println(".drop(0):" + List(1,2,3,4).drop(0))
//	println(".drop(3):" + List(1,2,3,4).drop(3))
//	println(List(1,2,3,4).drop(4))
//	println(List(1,2,3,4).drop(5))
//
//	println(".dropWhile x < 5")
//	println(List(1,2,3,4,5,6,7,8,9).dropWhile( x => x < 5  ) )
//	println(s"Init:%s=%s".format(List(1,2,3,4).toString,List(1,2,3,4).init).toString)
//	println(s"Length of ${List(1,2,3,4,5)} is ${List.length(List(1,2,3,4,5))})")
//
//	println(List.map(sampleList)(a => a*2))
//
//	println("Flatten:")
//	val listOfList = List( List(1,2,3,4),List(1,2,3,4,5),List(4,3,2,1) )
//	println(flatten(listOfList))
//
//	println(filter(sampleList)(a => a % 2 == 0))
//
//
//
//	//List(2,2,3,23,243,24,24,24,2,42,32,42,4,24,2,42,3,).foldLeft( (x:Int) => x*10 )((f,el) => p => f(el) + 1  )
//
//}

