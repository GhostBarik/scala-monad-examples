package main.scala

import scala.util.Random

object Main {

  // MONAD AND FUNCTOR INTERFACES

  trait Functor[F[_]] {

    def map[A,B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[M[_]] extends Functor[M] {

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def unit[A](a: A): M[A]
  }

  // AUXILIARY METHODS /////////////////////////////////////////////////////////

  // those helper methods will allows us to call some functions using infix notation
  // (i.e  `m flatMap f` instead of `flatMap(m)(f)`)

  implicit class FunctorOps[A, F[_] : Functor](obj: F[A]) {
    def map[B](f: A => B): F[B] = implicitly[Functor[F]].map(obj)(f)
  }

  implicit class MonadOps[A, M[_] : Monad](obj: M[A]) {
    def flatMap[B](f: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(obj)(f)
  }

  implicit class ParserOps[A](parser: ParserM[A]) {
    def or(other: ParserM[A]): ParserM[A] = orParser[A](parser, other)
  }

  // OPTION MONAD ///////////////////////////////////////////////////////////////

  sealed trait MyOption[+A]
  case object None extends MyOption[Nothing]
  case class Some[A](value: A) extends MyOption[A]

  implicit case object OptionAsMonad extends Monad[MyOption] {

    override def unit[A](a: A): MyOption[A] = Some(a)

    override def map[A, B](fa: MyOption[A])(f: A => B): MyOption[B] = fa match {
      case Some(x) => Some(f(x))
      case None    => None
    }

    override def flatMap[A, B](ma: MyOption[A])(f: A => MyOption[B]): MyOption[B] = {
      ma match {
        case Some(x) => f(x)
        case None    => None
      }
    }
  }

  // WRITER MONAD //////////////////////////////////////////////////////////////

  type Log = List[String]
  type Writer[A] = (A, Log)

  implicit case object WriterAsMonad extends Monad[Writer] {

    override def unit[A](a: A): (A, Log) = (a, List.empty)

    override def map[A, B](wa: Writer[A])(f: A => B): Writer[B] = {
      val (value, log) = wa
      (f(value), log)
    }

    override def flatMap[A, B](wa: Writer[A])(f: A => Writer[B]): Writer[B] = {
      val (value, log) = wa
      val (nextValue, updatedLog) = f(value)
      (nextValue, log ++ updatedLog)
    }
  }

  def log[A](logMessage: String)(value: A): Writer[A] = (value, List(logMessage))

  def plusWithLog[A](a: Int, b: Int): Writer[Int] = {
    log("let's add 2 values together") {
      a + b
    }
  }

  def mulptiplyWithLog[A](a: Int, b: Int): Writer[Int] = {
    log("let's multiply 2 values together") {
      a * b
    }
  }

  def computationWithLogging(a: Int, b: Int, c: Int): Writer[Int] =
    for {
      x <- plusWithLog(a, b)
      y <- mulptiplyWithLog(x, c)
    } yield {
      y
    }

  // RANDOM MONAD ////////////////////////////////////////////////////////////

  type S = Stream[Double]
  type RandomM[A] = S => (A, S)

  implicit case object RandomAsMonad extends Monad[RandomM] {

    override def unit[A](a: A): RandomM[A] = s => (a, s)

    override def map[A, B](fa: RandomM[A])(f: A => B): RandomM[B] = s => {
      val (newRandomValue, nextState) = fa(s)
      (f(newRandomValue), nextState)
    }

    override def flatMap[A, B](ma: RandomM[A])(f: A => RandomM[B]): RandomM[B] =
      s => {
        val (newRandomValue, state) = ma(s)
        f(newRandomValue)(state)
      }
  }

  // PARSER MONAD /////////////////////////////////////////////////

  type ParserState = List[Char]
  type ParserM[A] = ParserState => MyOption[(A, ParserState)]


  implicit case object ParserAsMonad extends Monad[ParserM] {

    override def unit[A](a: A): ParserM[A] = s => Some((a, s))

    override def map[A, B](fa: ParserM[A])(f: A => B): ParserM[B] = s =>
      fa(s) match {
        case Some((value, nextState)) => Some((f(value), nextState))
        case None => None
      }

    override def flatMap[A, B](ma: ParserM[A])(f: A => ParserM[B]): ParserM[B] = {
      s => {
        ma(s) match {
          case Some((value, nextState)) => f(value)(nextState)
          case None => None
        }
      }
    }
  }

  //  SOME SIMPLE PARSER EXAMPLES

  def parseChar(a: Char): ParserM[Char] = s => {
    if (s.nonEmpty && s.head == a) {
      Some((a, s.tail))
    } else {
      None
    }
  }

  // `orParser` is a parser combinator, which combines 2 parsers together
  // if first parser fails, the second one is run
  def orParser[A](p1: ParserM[A], p2: ParserM[A]): ParserM[A] = {
    s => {
      p1(s) match {
        case Some(res) => Some(res)
        case None => p2(s)
      }
    }
  }

  // empty parser, which parses nothing, just returns some single value
  def unit[A](a: A): ParserM[A] = s => Some((a, s))

  // zero parser parses nothing, just return empty list
  def zero: ParserM[List[Nothing]] = unit(List.empty)

  // apply parser zero or more times (notice mutual recursion!)
  def zeroOrMore[A](p1: ParserM[A]): ParserM[List[A]] = oneOrMore(p1) or zero

  // apply parser one or more times (notice mutual recursion!)
  def oneOrMore[A](p1: ParserM[A]): ParserM[List[A]] = {

    for {
      c <- p1
      tail <- zeroOrMore(p1)
    } yield {
      List(c) ++ tail
    }
  }


  def main(args: Array[String]): Unit = {

    /////////////////////////////////////////////////////

    val optionAsMonad = implicitly[Monad[MyOption]]
    val randomAsMonad = implicitly[Monad[RandomM]]

    import optionAsMonad.{unit => optionUnit}
    import randomAsMonad.{unit => randomUnit}

    /////////////////////////////////////////////////////

    val o1: MyOption[Int] = Some(10)
    val o2: MyOption[Int] = Some(30)
    val o3: MyOption[Int] = Some(40)

    val optionExample1: MyOption[Int] = for {
      x <- o1
      y <- o2
      z <- o3
    } yield {
      x + y + z
    }

    val optionExample2: MyOption[Int] =

      o1 flatMap (x =>
      o2 flatMap (y =>
        optionUnit(x + y)
      ))


    val optionExample3: MyOption[Int] =

      o1 flatMap (x =>
      o2 map     (y =>
        x + y
      ))

    // MONAD LAW: flatMap . unit = map

    println()
    println("********** OPTION MONAD EXAMPLE ****************")
    println()

    println(s"option example 1: $optionExample1")
    println(s"option example 2: $optionExample2")
    println(s"option example 3: $optionExample3")

    //////////////////////////////////////////////////

    val (result, log) = computationWithLogging(1, 2, 3)

    println()
    println("********** WRITER MONAD EXAMPLE ****************")
    println()
    println(s"value: $result")
    println(s"log: $log")

    // RANDOM MONAD EXAMPLE //////////////////////////

    val randomDouble:  RandomM[Double] = s => (s.head, s.tail)
    def randomBoolean: RandomM[Boolean] = randomDouble map (_ < 0.5)

    val values = List('A', 'B', 'C', 'D')

    def randomCharacter: RandomM[Char] = for {
      d <- randomDouble
    } yield {
      values((d * values.length).toInt)
    }

    def randomCharacterList(length: Int): RandomM[List[Char]] = for {
      c <- randomCharacter
      tail <- if (length-1 > 0) {
        randomCharacterList(length - 1)
      } else {
        randomUnit(List.empty)
      }

    } yield {
      List(c) ++ tail
    }

    def randomString(length: Int): RandomM[String] = randomCharacterList(length) map (_.mkString(""))
    def randomTimestamp: RandomM[Long] = randomDouble map (_ * 1000000.0) map (_.toLong)

    case class Event(guid: String, timestamp: Long, valid: Boolean)

    def randomEvent: RandomM[Event] = for {

      guid      <- randomString(16)
      timestamp <- randomTimestamp
      valid     <- randomBoolean

    } yield {

      Event(guid, timestamp, valid)
    }

    def randomEvents: RandomM[List[Event]] = for {

      event1 <- randomEvent
      event2 <- randomEvent
      event3 <- randomEvent

    } yield {
      List(event1, event2, event3)
    }

    /////////////////////////////////////////////////////////////////////////

    def generateInfiniteSequenceOfDoubles(seed: Int): Stream[Double] = {

      def generateInfiniteRandomSequence[T](generate: => T): Stream[T] = {
        def randomSequence(g: => T): Stream[T] = g #:: randomSequence(generate)
        randomSequence(generate)
      }

      val generator: Random = new Random(seed)
      generateInfiniteRandomSequence(generator.nextDouble())
    }

    // generate stream of preudo-randomly generated Doubles using the seed
    val randomDoubles = generateInfiniteSequenceOfDoubles(777)

    // pass infinite stream as a state and run monadic computation
    val (events, _) = randomEvents(randomDoubles)

    println()
    println("********** RANDOM MONAD EXAMPLE ****************")
    println()
    println(s"events: $events")
    println()
    println("*************************************************")


    val p = for {
      leftBracket  <- parseChar('{')             // parse left bracket
      a            <- zeroOrMore(parseChar('A')) // parse character 'A' multiple times (it could also be absent)
      b            <- oneOrMore(parseChar('B'))  // parse character 'B' multiple times (it should be present at least once)
      rightBracket <- parseChar('}')             // parse right bracket
    } yield {
      List(leftBracket) ++ a ++ b ++ List(rightBracket) // combine parsed results into single list
    }

    val inputText: List[Char] = "{AAB}".toList

    println()
    println("********** PARSER EXAMPLE ****************")
    println()

    p(inputText) match {
      case Some((parsingResult, _)) => println(s"parsing result: $parsingResult")
      case None => println(s"FAILED")
    }

    println()
    println("*************************************************")
  }
}
