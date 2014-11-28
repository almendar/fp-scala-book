import java.util.concurrent._

import fpinscala.ListA


val es : ExecutorService = Executors.newFixedThreadPool(10);

def st = new Callable[String] {
  val start = System.currentTimeMillis()
  override def call(): String = {
    Thread.sleep(10000)
    println(s"Sleeping time: ${System.currentTimeMillis()-start}")
    "Sleeping Thread"
  }
}

def nst = new Callable[String] {
  val start = System.currentTimeMillis()
  override def call(): String = {
    println(s"Non-sleeping time: ${System.currentTimeMillis()-start}")
    "Non-sleeping Thread"
  }
}

val stL =  ListA.fill(10)(st)
val nstL = ListA.fill(10)(nst)
val allstls = stL :: nstL
val all = ListA.map(allstls)(es.submit(_))
val p = ListA.map(all)(_.get)


//
//val lstCllStr : ListA[Callable[String]] = stL :: nstL
//
//val resultsF : ListA[Future[String]] = ListA.map(lstCllStr){ p : Callable[String] =>
//  es.submit(p)
//}
//
//
//
//
//val results = ListA.map(resultsF)(_.get)

