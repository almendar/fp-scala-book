package fpinscala

trait Monoid[A] {
	def op(a1 : A, a2 : A) : A
	def zero : A
}

object Monoid { 

	val stringMonoid = new Monoid[String] {
		override def op(a1: String, a2:String) : String = a1 + a2 
		override def zero : String = ""
	}

	val intAddition : Monoid[Int] = new Monoid[Int] {
		def op(i1:Int,i2 : Int) = i1 + i2
		def zero = 0
	}

	val intMultiplication: Monoid[Int] = new Monoid[Int] {
		def op(i1:Int,i2 : Int) = i1 * i2
		def zero = 1
	} 
	val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
		def op(i1:Boolean,i2 : Boolean) = i1 || i2
		def zero = false
	}
	val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
		def op(i1:Boolean,i2 : Boolean) = i1 && i2
		def zero = true
	}

	def optionMonoid[A] : Monoid[Option[A]] = new Monoid[Option[A]] {
		def op(i1 : Option[A], i2 : Option[A]) = i1 orElse i2
		def zero = None
	}

	def endoMonoid[A] : Monoid[A => A] = new Monoid[A => A] {
		def op(f : A => A, g : A=>A) = f compose g
		def zero : A => A = a => a 
	}

	def listMonoid[A] : Monoid[List[A]]= new Monoid[List[A]] {
		def op(x : List[A] , y : List[A]) = x ++ y
		def zero = Nil
	}

	def concatenate[A](as : List[A], m : Monoid[A]) : A = as.foldLeft(m.zero)(m.op)
}


trait Foldable[F[_]] {
	def foldRight[A, B](as: F[A])(f: (A, B) => B): B
	def foldLeft[A, B](as: F[A])(f: (B, A) => B): B
	def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
	// def concatenate[A](as: F[A])(m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
}


