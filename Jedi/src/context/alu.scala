package context


  
import expression._
import value._



/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args) //binary
      case "more" => more(args) // binary
      case "equals" => equals(args) // note: equals(7, true) = false, not error
      case "unequals" => unequals(args) // binary, = not(equals(args))?
      case "not" => not(args) // unary
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
  /** -attempt to cast Integer into Real <br> 
   *  -keep Real as Real
 * @param value single Value to be cast as Real
 * @param opcode used in TypeException
 * @return Real Value
 */
private def castAsReal(value: Value, opcode: String): Real = {
    value match {
      case n: Integer => Integer.intToReal(n)
      case n: Real => n
      case _ => throw new TypeException(opcode + " inputs must be numbers")
    }
  }
  
  /** keep Text as Text
 * @param value single Value to be cast as Text
 * @param opcode used in TypeException
 * @return Text Value
 */
private def castAsText(value: Value, opcode: String): Text = {
    value match {
      case n: Text => n
      case _ => throw new TypeException(opcode + " inputs must be texts")
    }
  }
  
  
  /** -if vals is empty, throw TypeException <br>
   *  -if one element is not isInstanceOf[Integer], throw TypeException <br>
   *  -cast all elements in vals to Integer
 * @param vals a list of values to be cast as Integer
 * @param opcode used in TypeException
 * @return List of Integer
 */
private def castAsIntegers(vals: List[Value], opcode: String): List[Integer] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Integer])
    if (ok.length < vals.length) throw new TypeException(opcode + " inputs must be numbers")
    vals.map(_.asInstanceOf[Integer])
  }  
  
  /** -if vals is empty, throw TypeException <br>
   *  -map element with castAscastAsReal(value: Value, opcode: String): Real
 * @param vals a list of values to be cast as Real
 * @param opcode used in TypeException
 * @return List of Real
 */
private def castAsReals(vals: List[Value], opcode: String): List[Real] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    vals.map(castAsReal(_, opcode))
  }
  
  /** -if vals is empty, throw TypeException <br>
   *  -map element with castAscastAsText(value: Value, opcode: String): Text
 * @param vals a list of values to be cast as Text
 * @param opcode used in TypeException
 * @return List of Text
 */
private def castAsTexts(vals: List[Value], opcode: String): List[Text] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    vals.map(castAsText(_, opcode))
  }
  
  /** -Try sum up all vals to Integer <br>
   *  -Try sum up all vals to Real <br>
   *  -Try concatenate all vals to Text <br>
   *  -Throw Exception if all fail
 * @param vals List of Value to be "added"
 * @return Value
 */
private def add(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "add").reduce(_+_) 
    } catch {
       case e: TypeException => 
         try {
           castAsReals(vals, "add").reduce(_+_) 
        } catch {
          case e: TypeException => castAsTexts(vals, "concat").reduce(_+_)
      }
    }
  }
  
  /** -Try multiply all vals to Integer <br>
   *  -Try multiply all vals to Real <br>
   *  -Throw Exception if all fail
 * @param vals List of Value to be multiplied
 * @return Value
 */
  private def mul(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "mul").reduce(_*_) 
    } catch {
       case e: TypeException => castAsReals(vals, "mul").reduce(_*_) 
    }
  }
  
  /** -Try subtract all vals to Integer <br>
   *  -Try subtract all vals to Real <br>
   *  -Throw Exception if all fail
 * @param vals List of Value to be subtracted
 * @return Value
 */
  private def sub(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "sub").reduce(_-_) 
    } catch {
       case e: TypeException => castAsReals(vals, "sub").reduce(_-_) 
    }
  }
  
  /** -Try divide all vals to Integer <br>
   *  -Try divide all vals to Real <br>
   *  -Throw Exception if all fail
 * @param vals List of Value to be divided
 * @return Value
 */
  private def div(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "div").reduce(_/_) 
    } catch {
       case e: TypeException => castAsReals(vals, "div").reduce(_/_) 
    }
  }
  
  /**
 * @param vals
 * @return
 */
def less(vals: List[Value]): Value = {
    if (vals.length  != 2) throw new TypeException("less expects two inputs")
    try {
      val nums = castAsIntegers(vals, "less")
      Boole(nums(0) < nums(1))
    } catch {
      case e: TypeException => {
        try {
          val nums = castAsReals(vals, "less")
          Boole(nums(0) < nums(1))
        } catch {
          case e: TypeException => {
            val texts = castAsTexts(vals, "less")
            Boole(texts(0) < texts(1))
          }
        }
      }
    }
  }
  
  def equal(vals: List[Value]): Value = {
    if (vals.length < 2) throw new TypeException("equal expects at least two inputs")
    try {
      val nums = castAsIntegers(vals, "less")
      Boole(nums(0) == nums(1))
    } catch {
      case e: TypeException => {
        try {
          val nums = castAsReals(vals, "less")
          Boole(nums(0) < nums(1))
        } catch {
          case e: TypeException => {
            val texts = castAsTexts(vals, "less")
            Boole(texts(0) < texts(1))
          }
        }
      }
    }
  }
 
   def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
   def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

  
  // etc.
}