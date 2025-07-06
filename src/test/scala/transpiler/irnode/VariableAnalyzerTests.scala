package transpiler.irnode

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.{should, shouldBe}
import transpiler.codegen.ir.{IRAssignment, IRBlock, IRLiteral, LiteralType}
import transpiler.codegen.scala.VariableAnalyzer


class VariableAnalyzerTests extends AnyFunSuite with Matchers {

  test("VariableAnalyzer should detect simple assignment") {
    val analyzer = new VariableAnalyzer()

    val ir = IRBlock(List(
      IRAssignment("x", IRLiteral("5", LiteralType.Integer))
    ))

    val result = analyzer.analyze(ir)
    val xInfo = result.variables("x")

    xInfo.isReassigned shouldBe false
    xInfo.needsHoisting shouldBe false
  }

  test("VariableAnalyzer should detect reassignment") {
    val analyzer = new VariableAnalyzer()
    val ir = IRBlock(List(
      IRAssignment("counter", IRLiteral("0", LiteralType.Integer)),
      IRAssignment("counter", IRLiteral("1", LiteralType.Integer))
    ))

    val result = analyzer.analyze(ir)

    val counterInfo = result.variables("counter")
    counterInfo.isReassigned shouldBe true
    counterInfo.needsHoisting shouldBe false
  }
}
