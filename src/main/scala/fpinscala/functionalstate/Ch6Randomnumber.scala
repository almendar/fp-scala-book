package fpinscala.functionalstate



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

  def double(rng:RNG) : (Double,RNG) = {
    val(i,rng1) = rng.nextInt;
    ((Math.abs(i).toDouble / Int.MaxValue),rng1)
  }

  def doubleElegang : Rand[Double] = map(_.nextInt)(a => a.toDouble / Int.MaxValue)

  def int : Rand[Int] = _.nextInt

  def intDouble:  Rand[(Int,Double)] = map2(int,doubleElegang)((a,b) => ((a,b)))
  def doubleInt: Rand[(Double,Int)] =  map2(doubleElegang,int)((a,b) => ((a,b)))
  def map2[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B) => C) : Rand[C] = { rng =>
    val (v1,r1) = ra.apply(rng)
    val (v2,r2) = rb.apply(r1)
    (f(v1,v2),r2)
  }


  def sequence[A](fs:List[Rand[A]]) : Rand[List[A]] = fs.foldRight(unit(List.empty[A]))((f,acc) => map2(f,acc)(_::_))

  def unit[A](a:A) : Rand[A] = rng => (a,rng)
  def map[A,B](s:Rand[A])(f : A => B) : Rand[B] = {
    rng =>
      val(a,rng2) = s(rng)
      (f(a),rng2)
  }
  def ints(count: Int): Rand[List[Int]] = RNG.sequence(List.fill(count)(int))
  def seedFromTime = simple(System.currentTimeMillis())
}

import RNG._

object RNGApp extends App {
  println(simplePair(seedFromTime)._1)
  println(double(seedFromTime)._1)
  println(doubleElegang(seedFromTime)._1)
  println(ints(6)(seedFromTime)._1)
  println(intDouble(simple(System.nanoTime()))._1)
  println(sequence(List(int,int,int,int,int)).apply(simple(System.nanoTime()))._1)
}
