package pli

class InterpreterSpec extends Spec {
  val interpreter: Interpreter = new Interpreter()

  def parser(text: String): Parser =
    Parser.forString(text.stripMargin)

  def eval(expression: Expression): Value = 
    interpreter.eval(expression)
   
  def run(text: String) {
    interpreter.run(toProgram(text))    
  }

  def expression(text: String): Expression =
    parser(text).parseExpression

  def statement(text: String): Statement =
    parser(text).parseStatement

  def toProgram(text: String): Program =
    parser(text).parseProgram

  "The interpreter" when "looking for expressions" should
    "accept positive integer literals" in {
      val result = eval(expression("123"))
      result should be(NumericValue(123))
    }

  it should "accept addition" in {
    val result = eval(expression("1+2"))
    result should be(NumericValue(3))
  }
  it should "accept substraction" in {
    val result = eval(expression("5-2"))
    result should be(NumericValue(3))
  }

  it should "accept multiplication" in {
    val result = eval(expression("1*3"))
    result should be(NumericValue(3))
  }
  it should "accept and" in {
    val result = eval(expression("true&true"))
    result should be(BooleanValue(true))
  }
  it should "accept andand" in {
    val result = eval(expression("true&&false"))
    result should be(BooleanValue(false))
  }

  it should "accept or" in {
    val result = eval(expression("true|false"))
    result should be(BooleanValue(true))
  }

  it should "accept oror" in {
    val result = eval(expression("true||false"))
    result should be(BooleanValue(true))
  }
  it should "accept equalequals" in {
    val result = eval(expression("true==false"))
    result should be(BooleanValue(false))
  }
  it should "accept concated addition" in {
    val result = eval(expression("1+2+3+4+5"))
    result should be(NumericValue(15))
  }
  it should "accept concated conjunctions" in {
    val result = eval(expression("true&true&true&true&false"))
    result should be(BooleanValue(false))
  }
  it should "priortize multiplication before addition" in {
    val result = eval(expression("1+2*2"))
    result should be(NumericValue(5))
  }
  it should "priortize multiplication before substraction" in {
    val result = eval(expression("3-2*2-1"))
    result should be(NumericValue(-2))
  }
  it should "priortize and before or" in {
    val result = eval(expression("true|false&true"))
    result should be(BooleanValue(true))
  }
  
  it should "accept and program without error" in {
    val lines = scala.io.Source.fromFile("testprogramm").mkString
    run(lines)
  }

}