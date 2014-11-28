package fpinscala

import Gen._

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')
  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))
  def advanceBy(n: Int) = copy(offset = offset+n)
  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}


case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}




trait Parsers[ParseError,Parser[+_]] { self =>

  def or[A](s1:Parser[A],s2: => Parser[A]) : Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def run[A](p : Parser[A])(input : String) : Either[ParseError,A]
  def map[A,B](a:Parser[A])(f: A => B) : Parser[B]
  def many[A](p: Parser[A]): Parser[List[A]]
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p,many(p))(_ :: _)
  def slice[A](p: Parser[A]): Parser[String]
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]
  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = product(p,p2) map {f.tupled}
  implicit def char(c: Char) : Parser[Char] = string(c.toString).map( (x : String) => x.charAt(0))
  implicit def string(s:String) : Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p:Parser[A]) {


    def |[B >:A](p2:Parser[B]) : Parser[B] = self.or(p,p2)
    def or[B>:A](p2:Parser[B]) : Parser[B] = self.or(p,p2)
    def map[B](f : A => B) : Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2 : Parser[B]) : Parser[(A,B)] = self.product(p,p2)
    def product[B](p2 : Parser[B]) : Parser[(A,B)] = self.product(p,p2)
  }

  object Laws {
    def equal[B](p1: Parser[B], p2: Parser[B])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[B](p: Parser[B])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }


}
