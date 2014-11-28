package fpinscala

import fpinscala.RNG.Rand

object Prop 
{
	type SuccessCount = Int
	type FailedCase = String
}

trait Status {
	def +(s:Status) : Status
}
case object Proven extends Status {
	override def +(s: Status): Status = s match {
		case Proven => Proven
		case Unfalsified => Unfalsified
	}
}
case object Unfalsified extends Status {
	override def +(s: Status): Status = Unfalsified
}

trait Prop 
{
	import Prop._

	def check : EitherA[FailedCase, (Status,SuccessCount)]
	def &&(p : Prop) : Prop = new Prop {
		override def check  = check.map2(p.check){case(s,c) => (s._1 + c._1, c._2 + s._2)}
	}
}


case class Gen[+A](sample : State[RNG,A], exhaustive : StreamA[Option[A]]) {

//	def flatMap[B](f: A => Gen[B]) : Gen[B] = Gen(sample.flatMap(a => f(a).sample),)

	def map[B](f: A => B): Gen[B] = Gen(sample.map(f),exhaustive.map(_.map(f)))
	def map2[B,C](g:Gen[B])(f: (A,B) => C) : Gen[C] = Gen(
		  for(i<-sample; j <- g.sample) yield f(i,j),
		  exhaustive.zip(g.exhaustive).map{case(p1,p2) => for(i<-p1;j<-p2) yield f(i,j)}
		)


}

object Gen {

	def unit[A](a: => A) : Gen[A] = Gen(State.unit(a), StreamA(Some(a)))
	def boolean : Gen[Boolean] = Gen(State(RNG.boolean), StreamA(true,false).map(Some(_)))
	// def listOf[A](a : Gen[A]) : Gen[List[A]] = ???
	def forAll[A](a: Gen[A])(f: A => Boolean) : Prop = ???

	def choose(start : Int, stopExclusive: Int): Gen[Int] = Gen(
			State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)),
			StreamA.from(start).take(stopExclusive - start).map(Some(_))
	)

	/** Between 0 and 1, not including 1. */
	def uniform: Gen[Double] = ???

	/** Between `i` and `j`, not including `j`. */
	def choose(i: Double, j: Double): Gen[Double] = ???

	def listOfN[A](n : Int, g:Gen[A]) : Gen[List[A]] = {
		State.sequence(List.fill(n)(g.sample))
		???
	}
	def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = ???

}