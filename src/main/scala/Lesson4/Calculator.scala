package Lesson4

/**
 * Created by rca733 on 5/7/16.
 */

import scala.collection.mutable.Stack

object Calculator {
  trait Operator{
    def operate(lhs:Int,rhs:Int):Int
  }
  object Operator{
    val operators: Map[String,Operator]= Map("+"-> Add,"-"-> Subtract,"*"-> Multiply,"/"->Divide)
    def unapply(token:String):Option[Operator]={
      operators.get(token)
    }
  }
  case object Add extends Operator{
    def operate(lhs:Int, rhs:Int):Int =lhs+rhs

  }
  case object Subtract extends Operator{
    def operate(lhs:Int, rhs:Int):Int =lhs+rhs

  }
  case object Divide extends Operator{
    def operate(lhs:Int, rhs:Int):Int =lhs/rhs

  }
  case object Multiply extends Operator{
    def operate(lhs:Int, rhs:Int):Int =lhs*rhs

  }
  object Number{
    def unapply(token:String): Option[Int] =try {
       Some(token.toInt)
    }
    catch{
      case _: NumberFormatException => None
    }
  }
  def handleNumber (token: String, stack:Stack[Int]):Boolean= try {
      stack.push(token.toInt)
      true
    }
    catch {
      case _ => false
    }
  sealed trait Expression
  case class NumberExpression(value: Int) extends Expression
  case class OperationExpression(lhs: Expression, rhs:Expression,op:Operator) extends Expression

// here we are using block for method
  def parse(expression : String):Expression={
    val stack = new Stack[Expression]
    // point to note we are not using '{' with for
    for(token <- expression.split(" ")) token match{
      case Number(num)=> stack.push(NumberExpression(num))
      case Operator(op)=>
        val rhs = stack.pop()
        val lhs = stack.pop()
        stack.push(OperationExpression(lhs,rhs,op))
      case _ => throw new IllegalArgumentException("invalid token "+token)
    }

    stack.pop()
  }
  def calculate(expression:Expression): Int = expression match{
    case NumberExpression(value)=> value
    case OperationExpression(lhs,rhs,op)=> op.operate(calculate(lhs),calculate(rhs))
  }
  def main(args:Array[String]):Unit=
    if(args.length !=1){
      throw new IllegalArgumentException("Usage: Calculator <expression> "+args.length)
    }
    else{
      val expression = parse(args(0))
      println("Result::::"+calculate(expression))
    }

}
