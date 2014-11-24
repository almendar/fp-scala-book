package fpinscala





object Prop 
{
	type SuccessCount = Int
	type FailedCase = String
}

trait Prop 
{
	import Prop._

	def check : EitherA[FailedCase, SuccessCount]
	def &&(p : Prop) : Prop = new Prop {
		override def check  = check.map2(p.check)(_ + _)
	}
}


case class Gen[+A](sample : State[RNG,A], exhaustive : StreamA[A])

object Gen {

	def unit[A](a: => A) : Gen[A] = Gen(State.unit(a), StreamA(a))
	def boolean : Gen[Boolean] = Gen(State(RNG.boolean), StreamA(true,false))
	// def listOf[A](a : Gen[A]) : Gen[List[A]] = ???
	def forAll[A](a: Gen[A])(f: A => Boolean) : Prop = ???

	def choose(start : Int, stopExclusive: Int): Gen[Int] = Gen(
			State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)),
			StreamA.from(start).take(stopExclusive - start)			
	)

	def listOfN[A](n : Int, g:Gen[A]) : Gen[List[A]] = ???

}