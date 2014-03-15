// This solve Berkely Programming Content #2: http://www.cs.berkeley.edu/~hilfingr/programming-contest/f2012-contest.pdf in Scala
object FractionMultiplying extends App {
  // val in constructor creates the getter methods only
  class Fraction(val numerator: Int, val denominator: Int) {
    override def toString = s"$numerator/$denominator"

    def isImproper = numerator >= denominator
    def normalized: Double = {
      if (isImproper)
        numerator.toDouble / denominator
      else
        denominator.toDouble / numerator
    }

    def *(that: Fraction): Fraction = {
      new Fraction(numerator * that.numerator, denominator * that.denominator)
    }

    def reduce: Fraction = {
      val gcdValue = gcd(numerator,denominator)
      new Fraction(numerator / gcdValue, denominator / gcdValue)
    }

    private def gcd(a: Int, b: Int): Int = {
      if (a == b)
        a
      else if (a > b)
        gcd(a - b, b)
      else
        gcd(a, b - a)
    }
  }

  class FractionParser {
    object ParserState extends Enumeration {
      type ParserState = Value
      val None, Numerator, Denominator = Value
    }
    import ParserState._

    class Transition(var transitionTriggerFn: (Char) => Boolean, var transitionFn: (Char) => Unit, var nextState: ParserState) { }
    class FiniteStateMachine {
      var transitions = scala.collection.mutable.Map[ParserState, Array[Transition]]()
      var currentState = ParserState.None
      var buffer = ""

      def register(current: ParserState, transitionTriggerFn: (Char) => Boolean, transitionFn: (Char) => Unit, nextState: ParserState) = {
        try {
          transitions(current) +:= new Transition(transitionTriggerFn, transitionFn, nextState)
        } catch {
          case e:java.util.NoSuchElementException => transitions(current) = Array[Transition](new Transition(transitionTriggerFn, transitionFn, nextState))
        }
      }

      def read(token: Char) = {
        val matchingTransition = transitions(currentState).find(_.transitionTriggerFn(token))
        if (matchingTransition.isDefined) {
          matchingTransition.get.transitionFn(token)
          currentState = matchingTransition.get.nextState
        }
      }
    }

    def consume(text: String) = {
      for(ch <- text.toArray) fsm.read(ch)
      fsm.read(' ')
    }

    var fractions = Array[Fraction]()
    val fsm = new FiniteStateMachine

    def isDigitToken(ch: Char) = ch.isDigit
    def appendToBuffer(token: Char) = (fsm.buffer += token)
    def emitFraction(token: Char) = {
      val parts = fsm.buffer.split('/')
      fractions +:= new Fraction(parts(0).toInt, parts(1).toInt)
      fsm.buffer = ""
    }

    fsm.register(ParserState.None, isDigitToken, appendToBuffer, ParserState.Numerator)
    fsm.register(ParserState.Numerator, isDigitToken, appendToBuffer, ParserState.Numerator)
    fsm.register(ParserState.Numerator, (token: Char) => token == '/', appendToBuffer, ParserState.Denominator)
    fsm.register(ParserState.Denominator, isDigitToken, appendToBuffer, ParserState.Denominator)
    fsm.register(ParserState.Denominator, (token: Char) => (token == ' '), emitFraction, ParserState.None)

    def multiplyAll: Fraction = {
      var cancellations = fractions.groupBy(_.normalized)
      var survivors = cancellations.filterKeys( cancellations(_).length == 1 )
      survivors.values.toList.flatten.foldLeft(new Fraction(1,1))((curr, product) => curr * product)
    }
  }

  var parser = new FractionParser
  parser.consume("1000/1 1000/2 1000/3 1000/4 1000/5 1/2       4/7        5/1000    4/1000  3/1000 2/1000       1/1000")
  println(parser.multiplyAll.reduce)
}
