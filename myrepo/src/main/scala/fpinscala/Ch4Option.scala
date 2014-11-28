package fpinscala

//import fpinscala.datastructures.list._

sealed trait OptionA[+A] {
	def map[B](f: A => B): OptionA[B]
	def flatMap[B](f: A => OptionA[B]): OptionA[B]
	def getOrElse[B >: A](default: => B): B
	def orElse[B >: A](ob: => OptionA[B]): OptionA[B]
	def filter(f: A => Boolean): OptionA[A]
}

case object NoneA extends OptionA[Nothing] {
	override def map[B](f: Nothing => B): OptionA[B] = NoneA
	override def filter(f: Nothing => Boolean): OptionA[Nothing] = NoneA
	override def orElse[B >: Nothing](ob: => OptionA[B]): OptionA[B] = ob
	override def getOrElse[B >: Nothing](default: => B): B = default
	override def flatMap[B](f: Nothing => OptionA[B]): OptionA[B] = NoneA
}

case class SomeA[A](value : A) extends OptionA[A] {
	override def map[B](f: A => B): OptionA[B] = SomeA(f(value))
	override def filter(f: A => Boolean): OptionA[A] = if(f(value)) this else NoneA
	override def orElse[B >: A](ob: => OptionA[B]): OptionA[B] = this
	override def getOrElse[B >: A](default: => B): B = value
	override def flatMap[B](f: A => OptionA[B]): OptionA[B] = f(value)

}

object Machers {

	import java.util.regex._

	def pattern(s: String): Option[Pattern] = try {
			Some(Pattern.compile(s))
		} catch {
			case e: PatternSyntaxException => None
		}

	def mkMatcher(pat: String): Option[String => Boolean] =
		pattern(pat) map (p => (s: String) => p.matcher(s).matches)

	def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = 
		OptionA.map2( mkMatcher(pat1), mkMatcher(pat2) ) ( (p1,p2) => p1(s) && p2(s) )

}

object OptionA {

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {i <- a; j <- b} yield f(i,j)
	def sequence[A](a: List[Option[A]]): Option[List[A]] = {
		a.foldLeft(Option(Nil):Option[List[A]]) { (acc,el) =>
			if(acc == None || el == None) None
			else Option(el.get :: acc.get)
		}
	}
	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a.map(f))
	
}
