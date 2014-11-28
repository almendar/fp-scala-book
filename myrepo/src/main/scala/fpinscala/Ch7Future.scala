package fpinscala

import java.util.concurrent.{Callable, TimeUnit}

import fpinscala.Par.Par

import scala.collection.immutable.IndexedSeq

object Util {
  def sum(as : IndexedSeq[Int]) : Par[Int] =
  	if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
	else {
		val (l,r) = as.splitAt(as.length/2)
		Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
	}
}


class ExecutorService {
	def submit[A](a: Callable[A]): Future[A] = ???
}

trait Future[A] {
	def get: A
	def get(timeout: Long, unit: TimeUnit): A
	def cancel(evenIfRunning: Boolean): Boolean
	def isDone: Boolean
	def isCancelled: Boolean
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a : => A) : Par[A] = ???
  def map2[A,B,C](p1 : Par[A], p2 : Par[B])(f : (A,B) => C) : Par[C] = ???
  def fork[A](a: => Par[A]) : Par[A] = ???
  def async[A](a : => A) : Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a : Par[A]) : A = ???
}


