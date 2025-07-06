package transpiler.irnode

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.{should, shouldBe}
import transpiler.TranspilerTestHelpers
import transpiler.codegen.ir.BinaryOp.{Eq, Ge, Gt}
import transpiler.codegen.ir.UnaryOp.{Minus, Not}
import transpiler.codegen.ir.*

class IfElseTest extends AnyFunSuite with Matchers with TranspilerTestHelpers:
  test("Simple if statement") {
    val python =
      """
x = 5
if x > 3:
    y = 1
        """
    val irNode = transpile(toPythonCode(python))

    irNode shouldBe IRBlock(
      List(IRAssignment("x", IRLiteral("5", LiteralType.Integer)),
        IRIf(IRBinaryOp(IRVariable("x"), Gt, IRLiteral("3", LiteralType.Integer)),
          IRBlock(List(IRAssignment("y", IRLiteral("1", LiteralType.Integer)))), None)))
  }

  test("If-else statement") {
    val python =
      """
a = 2
if a > 1:
    b = 10
else:
    b = 20
print(b)
        """
    val irNode = transpile(toPythonCode(python))
    irNode shouldBe IRBlock(
      List(
        IRAssignment("a",
        IRLiteral("2", LiteralType.Integer)),
        IRIf(
          IRBinaryOp(IRVariable("a"),Gt,IRLiteral("1", LiteralType.Integer)),
          IRBlock(List(IRAssignment("b",IRLiteral("10", LiteralType.Integer)))),
          Some(IRBlock(List(IRAssignment("b",IRLiteral("20", LiteralType.Integer)))))
        ),
        IRExprStmt(IRLiteral("print(b)", LiteralType.String)))
    )
  }

  test("If-elif-else statement") {
    val python =
      """
n = 0
if n > 0:
    sign = 1
elif n == 0:
    sign = 0
else:
    sign = -1
        """
    val irNode = transpile(toPythonCode(python))
    println(irNode)

    irNode shouldBe IRBlock(
      List(
        IRAssignment("n", IRLiteral("0", LiteralType.Integer)),
        IRIf(
          IRBinaryOp(IRVariable("n"), Gt, IRLiteral("0", LiteralType.Integer)),
          IRBlock(
            List(
              IRAssignment("sign", IRLiteral("1", LiteralType.Integer))
            )
          ),
          Some(
            IRIf(
              IRBinaryOp(IRVariable("n"), Eq, IRLiteral("0", LiteralType.Integer)), // this is invalid
              IRBlock(
                List(
                  IRAssignment("sign", IRLiteral("0", LiteralType.Integer)))
              ),
              Some(
                IRBlock(
                  List(IRAssignment("sign", IRUnaryOp(Minus, IRLiteral("1", LiteralType.Integer))))
                )
              )
            )
          )
        )
      )
    )
  }

  test("Nested if statement") {
    val python =
      """
score = 85
if score >= 60:
    if score >= 90:
        grade = "A"
    else:
        grade = "B"
else:
    grade = "F"
        """
    val statements = transpile(toPythonCode(python))

    statements shouldBe IRBlock(
      List(
        IRAssignment("score",IRLiteral("85", LiteralType.Integer)),
        IRIf(
          IRBinaryOp(
            IRVariable("score"), Ge, IRLiteral("60", LiteralType.Integer)),
          IRBlock(
            List(
              IRIf(
                IRBinaryOp(
                  IRVariable("score"), Ge, IRLiteral("90", LiteralType.Integer)),
                IRBlock(
                  List(IRAssignment("grade", IRLiteral("A", LiteralType.String)))
                ),
                Some(
                  IRBlock(
                    List(
                      IRAssignment("grade",IRLiteral("B", LiteralType.String)))
                  )
                )
              )
            )
          ),
          Some(
            IRBlock(
              List(
                IRAssignment("grade", IRLiteral("F", LiteralType.String)))
            )
          )
        )
      )
    )
  }

  test("If with boolean expression") {
    val python =
      """
x = True
if not x:
    y = False
        """
    val statements = transpile(toPythonCode(python))

    statements shouldBe IRBlock(
      List(
        IRAssignment("x",IRLiteral("true", LiteralType.Boolean)),
        IRIf(
          IRUnaryOp(Not, IRVariable("x")),
          IRBlock(
            List(
              IRAssignment("y",IRLiteral("false", LiteralType.Boolean)))),None)))
  }
