package fpinscala

sealed trait ListA[+A] {
	def head : A
	def tail : ListA[A]
	def drop(n:Int) : ListA[A] = n match {
		case 0 => this
		case n1 => tail.drop(n-1)
	}
	def dropWhile(f: A => Boolean): ListA[A] = if( f(head) ) tail.dropWhile(f) else ConsA(head,tail)
	def setHead[B >: A](b:B) : ListA[B] = ConsA(b,tail)
	def init : ListA[A] =  {
		tail match {
			case NilA => NilA
			case _ => ConsA(head,tail.init)
		}
	}

}

case object NilA extends ListA[Nothing] {
	def tail = NilA
	def head = throw new Exception("Empty list, cannot invoke .head on it")
	override def setHead[B](b:B) : ListA[B] = NilA
	override def drop(n:Int) = NilA
	override def init = NilA
	override def dropWhile(f: Nothing => Boolean) = NilA

}

case class ConsA[+A](_head: A, _tail: ListA[A]) extends ListA[A]  {
	def head = _head
	def tail = this._tail
}

object ListA {
	def sum(ints: ListA[Int]): Int = ints match {
			case NilA => 0
			case ConsA(x,xs) => x + sum(xs)
		}
	
	def product(ds: ListA[Double]): Double = ds match {
		case NilA => 1.0
		case ConsA(0.0, _) => 0.0
		case ConsA(x,xs) => x * product(xs)
	}

	def apply[A](as: A*): ListA[A] = {
		if (as.isEmpty) NilA
		else ConsA(as.head, apply(as.tail: _*))
}
	
	def incremantByOne(lst : ListA[Int]) : ListA[Int] = {
		NilA
	}

	def foldRight[A,B](l: ListA[A], z: B)(f: (A, B) => B): B =
		l match {
			case NilA => z
			case ConsA(x, xs) => f(x, foldRight(xs, z)(f))
		}

	def length[A](l:ListA[A]) : Int = {
		foldRight(l,0)( (a:A,b:Int) => b+1 )
	}

	@annotation.tailrec
	def foldLeft[A,B](l: ListA[A], z: B)(f: (B, A) => B): B = {
		l match {
			case NilA => z
			case ConsA(x,xs) => foldLeft(xs,f(z,x))(f)
		}
		
	}

	def reverse[A](l:ListA[A]) : ListA[A] = {
		foldLeft(l,NilA:ListA[A])((b,a) => ConsA(a,b))
	}

	def map[A,B](l: ListA[A])(f: A => B): ListA[B] = {
		reverse(foldLeft(l,NilA:ListA[B])((acc,el) => ConsA(f(el),acc)))
	}

	def flatten[A](ll:ListA[ListA[A]]) : ListA[A] = {
		reverse(foldLeft(ll,NilA:ListA[A])((acc,ell) => foldLeft(ell,acc)((acc1,el1)=>ConsA(el1,acc1))))
	}

	def filter[A](l:ListA[A])(f : A => Boolean) : ListA[A] = {
		reverse( foldLeft(l,NilA:ListA[A])( (acc,ell) => if(f(ell)) ConsA(ell,acc) else acc ) )
	}



	

}

//object Ch3ListA extends App {
//	val x = ListA(1,2,3,4,5) match {
//		case ConsA(x, ConsA(2, ConsA(4, _))) => x
//		case NilA => 42
//		case ConsA(x, ConsA(y, ConsA(3, ConsA(4, _)))) => x + y
//		case ConsA(h, t) => h + sum(t)
//		case _ => 101
//	}
//
//	val sampleListA = ListA(1,2,3,4,5)
//
//	println(x)
//	println(NilA)
//	println(ListA(1,2,3,4).tail)
//	println(".drop(0):" + ListA(1,2,3,4).drop(0))
//	println(".drop(3):" + ListA(1,2,3,4).drop(3))
//	println(ListA(1,2,3,4).drop(4))
//	println(ListA(1,2,3,4).drop(5))
//
//	println(".dropWhile x < 5")
//	println(ListA(1,2,3,4,5,6,7,8,9).dropWhile( x => x < 5  ) )
//	println(s"Init:%s=%s".format(ListA(1,2,3,4).toString,ListA(1,2,3,4).init).toString)
//	println(s"Length of ${ListA(1,2,3,4,5)} is ${ListA.length(ListA(1,2,3,4,5))})")
//
//	println(ListA.map(sampleListA)(a => a*2))
//
//	println("Flatten:")
//	val listOfListA = ListA( ListA(1,2,3,4),ListA(1,2,3,4,5),ListA(4,3,2,1) )
//	println(flatten(listOfListA))
//
//	println(filter(sampleListA)(a => a % 2 == 0))
//
//
//
//	//ListA(2,2,3,23,243,24,24,24,2,42,32,42,4,24,2,42,3,).foldLeft( (x:Int) => x*10 )((f,el) => p => f(el) + 1  )
//
//}

