package transpiler

import generated.{Python3Lexer, Python3Parser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.should

class TranspilerTest extends AnyFunSuite with Matchers {
  /**
   * Helper method to transpile Python code to Scala
   */
  def transpile(pythonCode: String): String = {
    val input = CharStreams.fromString(pythonCode)
    val lexer = new Python3Lexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new Python3Parser(tokens)

    // Suppress error output for cleaner tests
    parser.removeErrorListeners()

    val tree = parser.file_input()

    if (parser.getNumberOfSyntaxErrors > 0) {
      throw new RuntimeException(s"Parse errors in: $pythonCode")
    }

    val visitor = new PythonToScalaVisitor()
    visitor.visit(tree)
  }

  /**
   * Extract just the statements from generated code (remove object wrapper)
   */
  def extractStatements(scalaCode: String): List[String] = {
    val lines = scalaCode.split("\n").map(_.trim).filter(_.nonEmpty)
    val startIdx = lines.indexWhere(_.contains("def main"))
    val endIdx = lines.lastIndexWhere(_.contains("}"))

    if (startIdx >= 0 && endIdx > startIdx) {
      lines.slice(startIdx + 1, endIdx)
        .filterNot(_.startsWith("}"))
        .map(_.trim)
        .filter(_.nonEmpty)
        .toList
    } else {
      List.empty
    }
  }

  /**
  * Helper to convert multi-line string to valid Python code
   */
  private def toPythonCode(code: String): String = {
    code.trim.split('\n').map(_.trim).filter(_.nonEmpty).mkString("\n") + "\n"
  }

  private def transpileAndExtract(pythonCode: String): List[String] = {
    val validPython = toPythonCode(pythonCode)
    val scalaCode = transpile(validPython)
    extractStatements(scalaCode)
  }


  test("Single assignment should generate val") {
    val python = "x = 10\n"
    val statements = transpileAndExtract(python)

    statements should contain("val x = 10")
  }

  test("Multiple assignments should generate var for first, then assignment") {
    val python = "x = 10\nx = 5\n"
    val statements = transpileAndExtract(python)

    statements should have length 2
    statements should contain("var x = 10")
    statements should contain("x = 5")
  }

  test("Multiple assignments with three values") {
    val python =
      """
x = 1
x = 2
x = 3
  """
    val statements = transpileAndExtract(python)

    statements should have length 3
    statements should contain("var x = 1")
    statements should contain("x = 2")
    statements should contain("x = 3")
  }

  test("Different variables should be handled independently") {
    val python =
      """x = 10
  y = 20
  x = 15
  """
    val statements = transpileAndExtract(python)

    statements should have length 3
    statements should contain("var x = 10") // x is reassigned
    statements should contain("val y = 20") // y is not reassigned
    statements should contain("x = 15")
  }

  test("Complex example with multiple variables") {
    val python =
      """
  a = 1
  b = 2
  c = 3
  a = a + 1
  b = b * 2
  d = a + b + c
  """
    val statements = transpileAndExtract(python)

    statements should have length 6
    statements should contain("var a = 1") // a is reassigned
    statements should contain("var b = 2") // b is reassigned
    statements should contain("val c = 3") // c is not reassigned
    statements should contain("a = a + 1")
    statements should contain("b = b * 2")
    statements should contain("val d = a + b + c") // d is not reassigned
  }

  test("Boolean values should be converted") {
    val python =
      """flag1 = True
  flag2 = False
  """
    val statements = transpileAndExtract(python)

    statements should contain("val flag1 = true")
    statements should contain("val flag2 = false")
  }

  test("Arithmetic expressions should be preserved") {
    val python =
      """result = 5 + 3 * 2
  """
    val statements = transpileAndExtract(python)

    statements should contain("val result = 5 + 3 * 2")
  }

  test("Variable used in expression should work") {
    val python =
      """x = 10
  y = x + 5
  """
    val statements = transpileAndExtract(python)

    statements should contain("val x = 10")
    statements should contain("val y = x + 5")
  }

  test("Reassignment with expression should work") {
    val python =
      """counter = 0
  counter = counter + 1
  """
    val statements = transpileAndExtract(python)

    statements should contain("var counter = 0")
    statements should contain("counter = counter + 1")
  }

  test("None should be converted to null") {
    val python =
      """value = None
  """
    val statements = transpileAndExtract(python)

    statements should contain("val value = null")
  }

  // Edge cases
  test("Empty assignment should not crash") {
    val python = ""
    val statements = transpileAndExtract(python)

    statements should be(empty)
  }

  test("Numbers with different formats") {
    val python =
      """int_val = 42
  float_val = 3.14
  """
    val statements = transpileAndExtract(python)

    statements should contain("val int_val = 42")
    statements should contain("val float_val = 3.14")
  }

  test("String literals should be preserved") {
    val python =
      """name = "hello"
  message = 'world'
  """
    val statements = transpileAndExtract(python)

    statements should contain("val name = \"hello\"")
    statements should contain("val message = \"world\"")
  }
}
