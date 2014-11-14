package fpinscala

object ParticleModule extends App
{

  case class Pos(x:Double,y:Double,z:Double,fi:Double) {
    def pitagorian : Double = Math.exp(x*x + y*y + z*z)
    def +(other:Pos) : Pos = this.copy(x+other.x,y+other.y,z+other.z,fi+other.fi)
  }

  case class QuantumState(baseEnergy : Double,ripedMove:Double) {
    def nextState(p:Particle) : QuantumState = {
      val (value: Pos,nextState: QuantumState) = p.run(this)
      this.copy(baseEnergy = baseEnergy + value.pitagorian, ripedMove + value.pitagorian)
    }
  }

  val startingQuantumState = QuantumState(3,3)
  val startPos = Pos(1,2,3,4)

  val startParticle : Particle  = State( (s:QuantumState) => (startPos,startingQuantumState.nextState(startParticle)))


  type Particle = State[QuantumState,Pos]


  val p: State[QuantumState, Pos] = for {
    rp1 <- startParticle
    rp2 <- startParticle
  } yield rp1 + rp2

  println(p.run(startingQuantumState))


//  val ppp1: State[ParticleQuantum, ParticleQuantum] = randomParticle.flatMap(rp1 => randomParticle.flatMap(rp2 => State.get))

//   val ppp: State[ParticleQuantum, ParticleQuantum] = for {
//     rp1 <- randomParticle
//     rp2 <- randomParticle
//     p <- State.get
//   } yield p
//
//
//  randomParticle.run



}


object State
{
  def unit[S,A](a:A) : State[S,A] = State((s) => (a,s))
//  type State[S,+A] = S => (A,S)

  def sequence[S,A](lst : List[State[S,A]]) : State[S,List[A]] =
    lst.reverse.foldLeft(unit[S,List[A]](Nil))((b : State[S,List[A]],sa : State[S,A]) => sa.map2(b)(_::_))


  def get[S] : State[S,S] = State( (s1) => (s1,s1))

  def set[S](s : S) : State[S,Unit] = State(_ => ((),s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()
}


case class State[S,+A](run: S => (A,S))
{
  def map[B](f : A => B) : State[S,B] = flatMap((a) => State.unit(f(a)))

  def map2[B,C](sb : State[S,B])(f : (A,B) => C)  : State[S,C] = flatMap(a => sb.map((b) => f(a,b)))
//    for {
//    a <- this
//      b <- sb
//    } yield f(a,b)

    //
    // State((s) => run(s) match {case (a1,s1) => (f(a1),s1)})
//  def map2[B,C](sb : State[S,B])(f : (A,B) => C) : State[C,S] = State ( (s) => {
//      run(s) match {
//        case (a1,s1) => sb.run(s1) match {
//          case (b1,s2) => f(a1,b1)
//        }
//      }
//    })

  def flatMap[B](g : A => State[S,B]) : State[S,B] = State((s) => {
    val (a,s1) = run(s)
    g(a).run(s1)
  })


}

import fpinscala.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def rules(i:Input) = (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) =>
      Machine(false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) =>
      Machine(true, candy - 1, coin)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify(rules(i))))
    s <- get
  } yield (s.coins, s.candies)
}