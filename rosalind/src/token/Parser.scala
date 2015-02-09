package token

import scala.collection.mutable
import scala.util.matching.Regex

/**
 * Created by nikita on 16.12.14.
 */

object Parser {
  object TokenType extends Enumeration{
    type TokenType = Value
    val Value_, OpenBracket, Operator, CloseBracket = Value
  }

  import TokenType._
  case class Token(tokenType:TokenType, text:String)

  def tokenize(text:String):Seq[Token] = {
    val ReValue = """([\d.]+)""".r
    val ReOperation = """([\*\/\-\+])""".r
    text.split("\\s+").map {
      case ReValue(v) => Token(Value_, v)
      case ReOperation(v) => Token(Operator, v)
      case "(" => Token(OpenBracket, "(")
      case ")" => Token(CloseBracket, ")")
      case some => throw new Error("Invalid token: "+some)
    }
  }

  val str = "3 + ( 2 - 3.14 ) + 3"

  trait Expression {
    def debugString:String
    def evaluate:Float
    override def toString = debugString
  }

  class Value(val value:Float) extends Expression{
    def this(token:Token) = this(token.text.toFloat)
    override def debugString: String = value.toString

    override def evaluate: Float = value
  }

  class Brackets(val inside:Expression) extends Expression {
    override def debugString = s"(${inside.debugString})"

    override def evaluate: Float = inside.evaluate
  }

  class Operation(val operation:String, val left:Expression, val right:Expression) extends Expression{
    override def debugString = s"[$operation: ${left.debugString}, ${right.debugString}]"

    override def evaluate: Float = {
      val l = left.evaluate
      val r = right.evaluate
      operation match {
        case "+" => l + r
        case "-" => l - r
        case "*" => l * r
        case "/" => l / r
      }
    }
  }

  class Negate(val inside:Expression) extends Expression{
    override def debugString: String = s"[neg ${inside.debugString}]"

    override def evaluate: Float = -1 * inside.evaluate
  }
  object Operation{
    def isPriority(token:Token):Boolean = token.text == "*" || token.text == "/"
  }


  def parseValue(tokens:mutable.Queue[Token]):Expression = {
    val head = tokens.dequeue()
    head.tokenType match {
      case Value_ => new Value(head)
      case OpenBracket =>
        parseBracket(tokens)
//
//        parseOperation(brackets, tokens.tail)

      case _ => throw new Error("Invalid token: "+head)
    }
  }


  def parseOperation(in_left:Expression, tokens:mutable.Queue[Token]):Expression = {
//    println(s"parseOperation(${in_left}, ${tokens}")
    val left = if(in_left == null) {
      parseValue(tokens)
    } else {
      in_left
    }

    if(tokens.isEmpty || tokens.head.tokenType == CloseBracket){
      left
    }else{
      val opToken = tokens.dequeue()

      if(opToken.tokenType != Operator){
        throw new Error("Invalid token: "+opToken)
      }

      val right = parseValue(tokens)
//      println(s"left ${left.debugString} right ${right.debugString}")

      var op:Expression = null
      if(tokens.nonEmpty && Operation.isPriority(tokens.head)){
        val r = parsePriority(right, tokens)
        op = new Operation(opToken.text, left, r)
      } else{
        op = new Operation(opToken.text, left, right)
      }
      parseOperation(op, tokens)
    }
  }

  def parsePriority(left:Expression, tokens:mutable.Queue[Token]):Expression = {
//    println(s"parsePriority(left = ${left}, tokens = ${tokens}")
    if(tokens.isEmpty || !Operation.isPriority(tokens.head) ){
      left
    } else {
      val opToken = tokens.head

      if(opToken.tokenType != Operator){
        throw new Error("Invalid token: "+tokens.head)
      }

      tokens.dequeue()
      val right = parseValue(tokens)
      parsePriority(new Operation(opToken.text, left, right), tokens)
    }

  }

  def parseBracket(tokens:mutable.Queue[Token]):Brackets = {
    val br = new Brackets(parseOperation(null, tokens))
    if(tokens.isEmpty || tokens.dequeue().tokenType != CloseBracket){
      throw new Error("Close bracket not found")
    }
    br
  }

  def main(args: Array[String]) {
    val tokens = new mutable.Queue[Token]()

    tokens ++= tokenize("10 * 1 * ( 8 )")
    println(tokens)
    val ex = parseOperation(null, tokens)
    println(ex.debugString)
    println(ex.evaluate)
//    println(2f * 4f / 2f)
  }
}
