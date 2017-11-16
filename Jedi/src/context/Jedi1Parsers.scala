package context

import scala.util.parsing.combinator._
import expression._
import value._

/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {
   
   def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")
 
   def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
     case "def"~id~"="~exp => Declaration(id, exp)
   }
   
   def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
     case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
     case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
   }
 

   def  disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
     case con ~ Nil => con
     case con ~ more => Disjunction(con::more)
   }
   
   // conjunction ::= equality ~ ("&&" ~ equality)*
   def  conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
     case con ~ Nil => con
     case con ~ more => Conjunction(con::more)
   }
   
   // equality ::= inequality ~ ("==" ~ inequality)*
   def  equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
     case con ~ Nil => con
     case con ~ more => FunCall(Identifier("equals"), con::more)
   }
   
   // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
   
   // 

   
  // negate(exp) = 0 - exp
  private def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Integer(0)
    FunCall(sub, List(zero, exp))
  }
    
  // sum ::= product ~ ("+" | "-") ~ product)*  
  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product ^^ {
    case "+"~s=>s
    case "-"~s=> negate(s)
    })^^{
    case p~Nil=> p
    case p~rest=>FunCall(Identifier("add"), p::rest)
    }
    
 // product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term ^^ {
    case "*"~s=>s
    case "/"~s=> negate(s)
    })^^{
    case p~Nil=> p
    case p~rest=>FunCall(Identifier("mul"), p::rest)
    wut
    }
      
 def term: Parser[Expression]  = funCall | literal | "("~>expression<~")"
   
 def literal = boole | real | integer | text | identifier
   

 // text ::= any chars bracketed by quotes
 def text: Parser[Text] = """\"[^"]+\"""".r ^^ {
     case chars => Text(chars.substring(1, chars.length - 1))
 }
 
 // integer ::= 0|(\+|-)?[1-9][0-9]*
 def integer: Parser[Integer] = """\"[^"]+\"""".r ^^ {
     case chars => Integer(chars.substring(1, chars.length - 1))
 }
 
 // real ::= (\+|-)?[0-9]+\.[0-9]+
 
 // boole ::= true | false

 // identifier ::= [a-zA-Z][a-zA-Z0-9]*
 def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
   case chars => Identifier(chars)
 }
 
 // funCall ::= identifier ~ operands
 def funCall: Parser[FunCall] = identifier ~ operands
 
 // operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
  
}