package fpinscala

trait Functor[F[_]] {

	def map[A,B](fa: F[A])(f: A => B): F[B]
	def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
		(map(fab)(_._1), map(fab)(_._2))

}

trait Monad[M[_]] extends Functor[M] {
	def unit[A](a: => A): M[A]
	def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
	def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))
	def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = 
		flatMap(ma)(a => map(mb)(b => f(a,b)))

	def compose[A,B,C](f: A => M[B] , g: B => M[C]) : A => M[C] = (a:A) => 
		flatMap(f(a))(g)

}

object Ch11Monads extends App {
	val listFunctor = new Functor[List] {
		def map[A,B](fa: List[A])(f:A=>B) : List[B] = fa map f
	}

	val listMonad = new Monad[List] {
		def unit[A](a: => A) : List[A] = a :: Nil
		def flatMap[A,B](la : List[A])(f: A => List[B]) : List[B] = la flatMap f
	} 



	println(listMonad.compose((x:Int) => List.fill(x)(x), (y:Int) => List.fill(y)(y))(2))


	println(listFunctor.map(List(1,2,3,4))((x:Int) => x.toString))

	println(listFunctor.distribute(List((1.0,"1"),(2.0,"2"))))
}