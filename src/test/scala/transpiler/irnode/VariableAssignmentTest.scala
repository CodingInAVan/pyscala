package transpiler.irnode

import generated.{Python3Lexer, Python3Parser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.{should, shouldBe}
import transpiler.TranspilerTestHelpers
import transpiler.codegen.ir.BinaryOp.{Add, Div, Mul}
import transpiler.codegen.ir.LiteralType.Integer
import transpiler.codegen.ir.{IRAssignment, IRAugAssignment, IRBinaryOp, IRBlock, IRLiteral, IRVariable, LiteralType}

class VariableAssignmentTest extends AnyFunSuite with Matchers with TranspilerTestHelpers {

  test("Single assignment should generate val") {
    val python = "x = 10\n"
    val statements = transpile(toPythonCode(python))

    statements shouldBe IRBlock(
      List(
        IRAssignment("x", IRLiteral("10", Integer))
      )
    )
  }

  test("Multiple assignments should generate var for first, then assignment") {
    val python = "x = 10\nx = 5\n"
    val statements = transpile(toPythonCode(python))

    statements shouldBe IRBlock(
      List(
        IRAssignment("x",IRLiteral("10", Integer)),
        IRAssignment("x",IRLiteral("5", Integer))))
  }

  test("Simple augment assignment") {
    val python =
      """x *= 10 + a
    """
    val statements = transpile(toPythonCode(python))
    statements shouldBe IRBlock(
      List(
        IRAugAssignment("x", Mul,
          IRBinaryOp(
            IRLiteral("10", Integer), Add,
            IRVariable("a")
          )
        )
      )
    )
  }

  test("Multiple assignments with three values") {
    val python =
      """
x = 1 + 3 / 3
x *= 2
x = 3
  """
    val statements = transpile(toPythonCode(python))
    println(statements)
    statements shouldBe IRBlock(
      List(
        IRAssignment("x",
          IRBinaryOp(
            IRLiteral("1", Integer),
            Add,
            IRBinaryOp(
              IRLiteral("3", Integer),
              Div,
              IRLiteral("3", Integer)
            )
          )
        ),
        IRAugAssignment("x", Mul, IRLiteral("2", Integer)),
        IRAssignment("x", IRLiteral("3", Integer))))
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
    val statements = transpile(toPythonCode(python))

    statements shouldBe IRBlock(
      List(
        IRAssignment("a", IRLiteral("1", Integer)),
        IRAssignment("b",IRLiteral("2", Integer)),
        IRAssignment("c",IRLiteral("3", Integer)),
        IRAssignment("a", IRBinaryOp(IRVariable("a"), Add, IRLiteral("1",Integer))),
        IRAssignment("b", IRBinaryOp(IRVariable("b"),Mul,IRLiteral("2", Integer))),
        IRAssignment("d", IRBinaryOp(
          IRVariable("a"), Add,
          IRBinaryOp(IRVariable("b"),
            Add,IRVariable("c"))
        ))
      )
    )
  }

  test("Boolean values should be converted") {
    val python =
      """flag1 = True
  |flag2 = False
  """.stripMargin

    val statements = transpile(toPythonCode(python))
    statements shouldBe IRBlock(
      List(
        IRAssignment("flag1", IRLiteral("true", LiteralType.Boolean)),
        IRAssignment("flag2", IRLiteral("false", LiteralType.Boolean)))
    )
  }

  test("Arithmetic expressions should be preserved") {
    val python =
      """result = 5 + 3 * 2
  """
    val statements = transpile(toPythonCode(python))
  }

  test("Variable used in expression should work") {
    val python =
      """x = 10
  |y = x + 5
  """.stripMargin

    val statements = transpile(toPythonCode(python))
    statements shouldBe IRBlock(
      List(
        IRAssignment("x",
          IRLiteral("10", LiteralType.Integer)),
        IRAssignment("y", IRBinaryOp(IRVariable("x"),Add,IRLiteral("5", LiteralType.Integer)))))
  }

  test("Reassignment with expression should work") {
    val python =
      """counter = 0
  |counter = counter + 1
  """.stripMargin

    val statements = transpile(toPythonCode(python))
    statements shouldBe IRBlock(
      List(
        IRAssignment("counter", IRLiteral("0", LiteralType.Integer)),
        IRAssignment("counter", IRBinaryOp(IRVariable("counter"), Add, IRLiteral("1", LiteralType.Integer)))
      )
    )
  }

  test("None should be converted to null") {
    val python =
      """value = None
  """
    val statements = transpile(toPythonCode(python))
    statements shouldBe IRBlock(List(IRAssignment("value", IRLiteral("null", LiteralType.Null))))
  }

  // Edge cases
  test("Empty assignment should not crash") {
    val python = ""
    val statements = transpile(toPythonCode(python))
    statements shouldBe IRBlock(List())
  }

  test("Numbers with different formats") {
    val python =
      """int_val = 42
  float_val = 3.14
  """
    val statements = transpile(toPythonCode(python))
  }

  test("String literals should be preserved") {
    val python =
      """name = "hello"
  |message = 'world'
  """.stripMargin
    val statements = transpile(toPythonCode(python))

    statements shouldBe IRBlock(
      List(
        IRAssignment("name",IRLiteral("hello", LiteralType.String)),
        IRAssignment("message",IRLiteral("world", LiteralType.String))))
  }
}
