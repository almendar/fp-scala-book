package fpinscala

sealed trait EitherA[+E,+A] {
	def map[B](f: A => B): EitherA[E, B] = this match {
		case RightA(a) => RightA(f(a))
		case LeftA(e) => LeftA(e)
	}

def flatMap[EE >: E, B](f: A => EitherA[EE, B]): EitherA[EE, B] = this match {
	case LeftA(e) => LeftA(e)
	case RightA(a) => f(a)
}

def orElse[EE >: E, AA >: A](b: => EitherA[EE, AA]): EitherA[EE, AA] = this match {
	case LeftA(_) => b
	case RightA(a) => RightA(a)
}

def map2[EE >: E, B, C](b: EitherA[EE, B])(f: (A, B) => C): EitherA[EE, C] = 
	for { a <- this; b1 <- b } yield f(a,b1)
}

case class LeftA[+E](get: E) extends EitherA[E,Nothing]
case class RightA[+A](get: A) extends EitherA[Nothing,A]

object EitherA {
	def mean(xs: IndexedSeq[Double]): EitherA[String, Double] =
	if (xs.isEmpty) LeftA("mean of empty list!")
	else RightA(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): EitherA[Exception, Int] =
	try RightA(x / y)
	catch { case e: Exception => LeftA(e) }

	def Try[A](a: => A): EitherA[Exception, A] =
		try RightA(a)
		catch { case e: Exception => LeftA(e) }

def traverse[E,A,B](es: List[A])(f: A => EitherA[E, B]): EitherA[E, List[B]] = es match {
	case Nil => RightA(Nil)
	case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
}

def traverse_1[E,A,B](es: List[A])(f: A => EitherA[E, B]): EitherA[E, List[B]] = 
	es.foldRight[EitherA[E,List[B]]](RightA(Nil))((a, b) => f(a).map2(b)(_ :: _))

def sequence[E,A](es: List[EitherA[E,A]]): EitherA[E,List[A]] = traverse(es)(x => x)
/*
There are a number of variations on `Option` and `EitherA`. If we want to accumulate multiple errors, a simple approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:
trait Partial[+A,+B]
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]
There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`, `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing values into a list; we can accumulate values using any user-supplied binary function.
It's also possible to use `EitherA[List[E],_]` directly to accumulate errors, using different implementations of helper functions like `map2` and `sequence`.
*/
}