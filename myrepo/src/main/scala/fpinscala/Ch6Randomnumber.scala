package fpinscala

case class ComplexNumber(x1:Double,x2:Double)

trait RNG {
  def nextInt: (Int,RNG)
}

object RNG {

  type Rand[A] = RNG => (A,RNG)


  def simple(seed : Long) : RNG = new RNG {
    override def nextInt: (Int, RNG) = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
      simple(seed2))
    }
  }


  def simplePair(rng:RNG) : ((Int,Int),RNG) = {
    val (i1,rng2) = rng.nextInt;
    val (i2,rng3) = rng2.nextInt;
    ((i1,i2),rng3)
  }



  def double : Rand[Double] = map(_.nextInt)(a => a.toDouble / Int.MaxValue)

  def int : Rand[Int] = _.nextInt

  def boolean : Rand[Boolean] = map(int)(a => a%2==0)
  
  def nonNegativeInt : Rand[Int] = map(_.nextInt)(x => if(x<0) -1*x else x)
  
  def complexNumber : Rand[ComplexNumber] = map(doubldobule)((x) => ComplexNumber(x._1,x._2))

  def intDouble:  Rand[(Int,Double)] = map2(int,double)((a,b) => ((a,b)))
  
  def doubleInt: Rand[(Double,Int)] =  map2(double,int)((a,b) => ((a,b)))
  
  def intin: Rand[(Int,Int)] = map2(int,int)((_,_))
  
  def doubldobule : Rand[(Double,Double)] = map(intin)( (x) =>  ((x._1.toDouble / Int.MaxValue),(x._2.toDouble / Int.MaxValue) )) 
  
  def map2[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B) => C) : Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))

//  { rng =>
//    val (v1,r1) = ra.apply(rng)
//    val (v2,r2) = rb.apply(r1)
//    (f(v1,v2),r2)
//  }


  def map[A,B](s:Rand[A])(f : A => B) : Rand[B] = flatMap(s)((a) => unit(f(a)))

//  {
//    rng =>
//      val(a,rng2) = s(rng)
//      (f(a),rng2)
//  }



  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a,rng1) = f(rng)
    g(a)(rng1)
  }


  def sequence[A](fs:List[Rand[A]]) : Rand[List[A]] = fs.foldRight(unit(List.empty[A]))((f,acc) => map2(f,acc)(_::_))

  def unit[A](a:A) : Rand[A] = rng => (a,rng)
  

  def ints(count: Int): Rand[List[Int]] = RNG.sequence(List.fill(count)(int))
  def seedFromTime : RNG = simple(System.currentTimeMillis())
}

import fpinscala.RNG._

object RNGApp extends App {
  println(simplePair(seedFromTime)._1)
  println(double(seedFromTime)._1)
  println(double(seedFromTime)._1)
  println(ints(6)(seedFromTime)._1)
  println(intDouble(simple(System.nanoTime()))._1)
  println(sequence(List(int,int,int,int,int)).apply(simple(System.nanoTime()))._1)
  println(sequence(List(complexNumber,complexNumber,complexNumber)).apply(seedFromTime))
}
