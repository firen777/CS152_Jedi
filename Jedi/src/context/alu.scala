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
  /**default ALU function
 * @param opcode Identifier
 * @param args
 * @return
 */
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
  
  /** less than comparison<br><p>
   *  -Throw TypeException if vals.length != 2<br>
   *  -Try cast vals into List of Integer then compare them<br>
   *  -Try cast vals into List of Real then compare them<br>
   *  -Try cast vals into List of Text then compare them (see String comparison)<br>
   *  -Throw TypeException if all fail</p>
 * @param vals a pair of Value to be compared
 * @return Boole vals(0) &lt; vals(1)
 */
private def less(vals: List[Value]): Value = {
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
  
  /** greater than comparison<br><p>
   *  -Throw TypeException if vals.length != 2<br>
   *  -Try cast vals into List of Integer then compare them<br>
   *  -Try cast vals into List of Real then compare them<br>
   *  -Try cast vals into List of Text then compare them (see String comparison)<br>
   *  -Throw TypeException if all fail</p>
 * @param vals a pair of Value to be compared
 * @return Boole vals(0) &gt; vals(1)
 */
private def more(vals: List[Value]): Value = {
    if (vals.length  != 2) throw new TypeException("more expects two inputs")
    try {
      val nums = castAsIntegers(vals, "more")
      Boole(nums(0) > nums(1))
    } catch {
      case e: TypeException => {
        try {
          val nums = castAsReals(vals, "more")
          Boole(nums(0) > nums(1))
        } catch {
          case e: TypeException => {
            val texts = castAsTexts(vals, "more")
            Boole(texts(0) > texts(1))
          }
        }
      }
    }
  }
  
  /** -throw TypeException if vals has less than 2 elements <br>
   *  -return if all elements are equal. Note mismatch type return false instead of Error
   *  
 * @param vals List of Value to be compared
 * @return all elements in vals are equal
 */
  private def equals(vals: List[Value]): Value = {
    if (vals.length < 2) throw new TypeException("equals expects at least two inputs")
    // Type mismatch, return false
//    if (vals.filter(_.isInstanceOf[Integer]).length != vals.length) Boole(false)
//    if (vals.filter(_.isInstanceOf[Real]).length != vals.length) Boole(false)
//    if (vals.filter(_.isInstanceOf[Text]).length != vals.length) Boole(false)
    Boole(vals.distinct.length==1)
  }
  
  /** -Throw TypeException if vals.length != 2<br>
   *  -return !equals(vals)
   *  
 * @param vals List of Value to be compared
 * @return elements in vals are not equal
 */
  private def unequals(vals: List[Value]): Value = {
    if (vals.length  != 2) throw new TypeException("unequals expects two inputs")
    !(equals(vals).asInstanceOf[Boole])
  }
  
  /** -Throw TypeException if vals.length != 2<br>
   *  -Throw TypeException if vals is not type Boole<br>
   *  -return !vals(0) 
   *  
 * @param vals the Value to be negate
 * @return vals(0)'s negation
 */
  private def not(vals: List[Value]): Value = {
    if (vals.length  != 1) throw new TypeException("not expects one input")
    if (!vals(0).isInstanceOf[Boole]) throw new TypeException("not expects Boole as input")
    !vals(0).asInstanceOf[Boole]
  }

 
   def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
   def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

  
  // etc.
}