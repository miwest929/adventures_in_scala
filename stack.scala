// Creates a simple RPN evaluator in Scala
object RPNEvaluator extends App {
  class MyStack {
    private var data = Array[Int]()

    def push(item: Int) = data +:= item
    def isEmpty: Boolean = data.length == 0
    def pop = {
      val topItem = data(0)
      data = data.drop(1)
      topItem
    }
  }

  class RPN {
    private var stack = new MyStack

    private def processToken(token: Char) = token match {
      case '+' => performAdd
      case '-' => performSubtract
      case '*' => performMultiply
      case '/' => performDivide
      case _ => stack.push(token.asDigit)
    }
    private def performGeneric(op: (Int, Int) => Int) = {
      val op1 = stack.pop
      val op2 = stack.pop
      stack.push( op(op2, op1) )
    }
    private def performAdd = performGeneric(_ + _)
    private def performSubtract = performGeneric(_ - _)
    private def performMultiply = performGeneric(_ * _)
    private def performDivide = performGeneric(_ / _)

    def evaluate(eq: String): Int = {
      for (ch <- eq.toArray) processToken(ch)
      stack.pop
    }
  }

  val rpn = new RPN
  println(rpn.evaluate("512+4*+3-"))
}
