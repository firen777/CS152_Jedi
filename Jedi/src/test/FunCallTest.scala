package test

import expression._
import context._
import value._


object FunCallTest extends App {
  val globalEnvironment = new Environment
  val operands = List(Integer(6), Integer(7))
  var exp = FunCall(Identifier("add"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("less"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("mul"), operands)
  println(exp.execute(globalEnvironment))
  
  val operands2 = List(Integer(6), Integer(6), Integer(6), Integer(6), Integer(6))
  exp = FunCall(Identifier("equals"), operands2)
  println(exp.execute(globalEnvironment))
  
  val operands3 = List(Integer(6), Real(6.0))
  exp = FunCall(Identifier("equals"), operands3)
  println(exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("not"), List(exp))
  println(exp.execute(globalEnvironment))
}