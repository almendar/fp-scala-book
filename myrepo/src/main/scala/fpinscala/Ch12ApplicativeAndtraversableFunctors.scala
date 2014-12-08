package fpinscala

trait Applicative[F[_]] extends Functor[F] {

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  /*
  map2 and apply are interchangeable
   */
  def map2[A,B,C](fa : F[A],fb : F[B])(f : (A,B) => C) : F[C] = apply(apply(unit(f.curried))(fa))(fb)
  def apply[A,B](fab : F[A=>B])(fa:F[A]) : F[B]// = map2(fab,fa)((f,arg) => f(arg))
  def unit[A](a:A) : F[A]

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =  as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  def sequence[A](fas : List[F[A]]) : F[List[A]] = traverse(fas)(x => x )
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_,_))

  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  /**
   * (A,B) => C
   */
}


object Ch12ApplicativeAndtraversableFunctors extends App {






  val optionApplicative = new Applicative[Option] {

    override def apply[A, B](fab: Option[(A) => B])(fa: Option[A]): Option[B] =
      (fab,fa) match  {
        case (Some(f),Some(a)) => Some(f(a))
        case _ => None
      }

    override def unit[A](a: A): Option[A] = Some(a)
  }

  val f: (Int, Int, Int) => Int = _ + _ + _
  val g: (Int) => (Int) => (Int) => Int = f.curried
  val h: Option[(Int) => (Int) => (Int) => Int] = optionApplicative.unit(g)

//  val  p = optionApplicative.map3(Some(1),Some(2),Some(3))(g)



}
